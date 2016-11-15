package util

import application.compilerCockpit.{MarkOutputGrammar, PrettyPrint}
import core.particles._
import core.particles.node.{ComparisonOptions, Node}
import org.scalatest.FunSuite
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.{ByteCodeMethodInfo, ByteCodeSkeleton, PrintByteCode}
import transformations.javac.JavaCompiler
import scala.reflect.io.{Directory, File, Path}
import scala.sys.process.{Process, ProcessLogger}

object TestUtils extends TestUtils(JavaCompiler.getCompiler) {
}

class TestUtils(val compiler: CompilerFromParticles) extends FunSuite {

  def toFile(program: String): String = {
    val fileName = File.makeTemp(suffix = ".java")
    val file = fileName.createFile()
    file.writeAll(program)
    fileName.toString()
  }

  def currentDir = new File(new java.io.File("."))
  def rootOutput = currentDir / Path("testOutput")
  def actualOutputDirectory = rootOutput / "actual"

  def testInstructionEquivalence(expectedByteCode: Node, compiledCode: Node) {
    for (methodPair <- ByteCodeSkeleton.getMethods(expectedByteCode).zip(ByteCodeSkeleton.getMethods(compiledCode))) {
      assert(new ComparisonOptions(false, true, false).deepEquality(getMethodInstructions(methodPair._1), getMethodInstructions(methodPair._2)))
    }
  }

  def getMethodInstructions(method: Node) =
    CodeAttribute.getCodeInstructions(ByteCodeMethodInfo.getMethodAttributes(method).head)

  def printByteCode(byteCode: Node): String = {
    PrintByteCode.printBytes(getBytes(byteCode))
  }

  def getBytes(byteCode: Node): Seq[Byte] = {
    var output: Seq[Byte] = null
    val particles: Seq[Delta] = Seq(new GetBytes(s => output = s)) ++ JavaCompiler.byteCodeTransformations
    new CompilerFromParticles(particles).transform(byteCode)
    output
  }

  def runByteCode(className: String, code: Node, expectedResult: Int) {
    val line = runByteCode(className, code)
    assertResult(expectedResult)(Integer.parseInt(line))
  }

  def runByteCode(className: String, code: Node) : String = {
    val bytes = getBytes(code).toArray
    val currentDir = new File(new java.io.File("."))
    val testDirectory = currentDir / Path("testOutput")
    testDirectory.createDirectory()
    val byteCodeFile = File.apply(testDirectory / Path(className).addExtension("class"))
    val writer = byteCodeFile.outputStream(append = false)
    writer.write(bytes)
    writer.close()

    runJavaClass(className, testDirectory)
  }

  def parseAndTransform(className: String, inputDirectory: Path): Node = {
    val input: File = getJavaTestFile(className, inputDirectory)
    compiler.parseAndTransform(input).program
  }

  def getJavaTestFile(fileName: String, inputDirectory: Path = Path("")): File = {
    val className = fileNameToClassName(fileName)
    val relativeFilePath = inputDirectory / (className + ".java")
    getTestFile(relativeFilePath)
  }

  def getTestFile(relativeFilePath: Path): File = {
    if (relativeFilePath.isAbsolute)
      return File(relativeFilePath)

    val fullPath = relativeFilePath
    var testResources = ClassLoader.getSystemResource("/" + fullPath.path)
    if (testResources == null)
      testResources = getClass.getResource("/" + fullPath.path)
    if (testResources == null)
    {
      throw new RuntimeException(s"Test file ${fullPath.path} not found")
    }
    File(testResources.getPath)
  }

  def compileAndRun(fileName: String, inputDirectory: Path = Path("")): String = {
    val className: String = fileNameToClassName(fileName)
    val relativeFilePath = inputDirectory / (className + ".java")
    val currentDir = new File(new java.io.File("."))
    val testOutput = Directory(currentDir / Path("testOutput"))
    val input: File = getTestFile(relativeFilePath)
    compiler.compile(input, Directory(Path(testOutput.path) / inputDirectory))
    val qualifiedClassName: String = (inputDirectory / className).segments.reduce[String]((l, r) => l + "." + r)
    TestUtils.runJavaClass(qualifiedClassName, testOutput)
  }

  def compileAndPrettyPrint(fileName: File): String = {

    val prettyPrint = PrettyPrint(recover = true)
    val splicedParticles = compiler.replace(MarkOutputGrammar,Seq(prettyPrint))
    val newCompiler = new CompilerFromParticles(splicedParticles)

    val state = newCompiler.parseAndTransform(fileName)
    state.output
  }

  def compareWithJavacAfterRunning(fileName: String, inputDirectory: Path = Path("")) {
    val className = fileNameToClassName(fileName)

    val relativeFilePath = inputDirectory / (className + ".java")
    val input: File = getTestFile(relativeFilePath)

    val expectedOutputDirectory = rootOutput / "expected"
    expectedOutputDirectory.createDirectory()
    val javaCompilerOutput = runJavaC(currentDir, input, expectedOutputDirectory)
    assertResult("")(javaCompilerOutput)

    val state = compiler.compile(input, Directory(actualOutputDirectory / inputDirectory))
    val qualifiedClassName: String = (inputDirectory / Path(className)).segments.reduce[String]((l, r) => l + "." + r)

    val expectedOutput = TestUtils.runJavaClass(qualifiedClassName, expectedOutputDirectory)
    try {
      val actualOutput = TestUtils.runJavaClass(qualifiedClassName, actualOutputDirectory)
      assertResult(expectedOutput)(actualOutput)
    }
    catch {
      case e: AssertionError =>
        throw new AssertionError(getErrorMessage(className, inputDirectory, expectedOutputDirectory, state, e))
    }
  }

  def fileNameToClassName(fileName: String): String = {
    if (fileName.endsWith(".java")) fileName.dropRight(5) else fileName
  }

  def getErrorMessage(className: String, inputDirectory: Path, expectedOutputDirectory: Path, state: CompilationState, e: AssertionError): String = {
    val relativeClassPath = inputDirectory / (className + ".class")
    val actualByteCodeAccordingToJavap = runJavaP((actualOutputDirectory / relativeClassPath).toFile)
    val expectedByteCodeAccordingToJavap = runJavaP((expectedOutputDirectory / relativeClassPath).toFile)

    val prettyPrintByteCodeCompiler = new CompilerFromParticles(Seq(new PrettyPrint) ++ JavaCompiler.byteCodeTransformations)
    val prettyPrintState = prettyPrintByteCodeCompiler.transformReturnState(state.program)
    val prettyPrintActualByteCode = prettyPrintState.output

    val originalMessage = e.getMessage
    val errorMessage: String = s"Output comparison failed with message: \n$originalMessage \n\n" +
      s"javac byteCode was: \n\n$expectedByteCodeAccordingToJavap \n\n" +
      s"actual byteCode according to javap was: \n$actualByteCodeAccordingToJavap \n\n" +
      s" actual byteCode according to prettyPrint was: \n$prettyPrintActualByteCode"
    (rootOutput / "outputLog.txt").createFile().writeAll(errorMessage)
    errorMessage
  }

  def runJavaP(input: File): String = {
    val processBuilder = Process.apply(s"javap -v $input")
    var line: String = ""
    val logger = ProcessLogger(
      (o: String) => line += o + "\n",
      (e: String) => line += e + "\n")
    val exitValue = processBuilder ! logger
    assertResult(0, line)(exitValue)
    line
  }

  def runJavaC(directory: Path, input: File, output: Path): String = {
    val processBuilder = Process.apply(s"javac -d $output $input", directory.jfile)
    var line: String = ""
    val logger = ProcessLogger(
      (o: String) => line += o,
      (e: String) => line += e)
    val exitValue = processBuilder ! logger
    assertResult(0, s"Java compiler did not exit successfully.\nMessage was $line")(exitValue)
    line
  }


  def runJavaClass(className: String, directory: Path): String = {
    val processBuilder = Process.apply(s"java $className", directory.jfile)
    var line: String = ""
    val logger = ProcessLogger(
      (o: String) => line += o,
      (e: String) => line += e)
    val exitValue = processBuilder ! logger
    line
  }

  def compareConstantPools(expectedByteCode: Node, compiledCode: Node) {
    val expectedConstantPoolSet = expectedByteCode.constantPool.constants
    val compiledConstantPoolSet = compiledCode.constantPool.constants
    assertResult(expectedConstantPoolSet.length)(compiledConstantPoolSet.length)
    assert(expectedConstantPoolSet.forall(expectedItem => {
      val hasEquivalent = compiledConstantPoolSet.exists(compiledItem =>
        ComparisonOptions(compareIntegers = false,takeAllLeftKeys = false,takeAllRightKeys = true).deepEquality(compiledItem, expectedItem))
      hasEquivalent
    }))
  }

  class GetBytes(write: Seq[Byte] => Unit) extends DeltaWithPhase {
    override def transform(program: Node, state: CompilationState): Unit = {
      write(PrintByteCode.getBytes(program, state))
    }

    override def description: String = "Writes the current program as JVM class file bytes to a function."
  }

}
