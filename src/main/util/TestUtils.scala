package util

import core.transformation._
import core.transformation.sillyCodePieces.{Injector, ProgramTransformation}
import org.junit.Assert
import transformations.bytecode.{CodeAnnotation, ByteCodeSkeleton, PrintByteCode}
import transformations.javac.JavaCompiler

import scala.reflect.io.{Directory, File, Path}
import scala.sys.process.{Process, ProcessLogger}

object TestUtils extends TestUtils(JavaCompiler.getCompiler)

class TestUtils(val compiler: CompilerFromTransformations) {

  def testInstructionEquivalence(expectedByteCode: MetaObject, compiledCode: MetaObject) {
    for (methodPair <- ByteCodeSkeleton.getMethods(expectedByteCode).zip(ByteCodeSkeleton.getMethods(compiledCode))) {
      Assert.assertTrue(MetaObject.deepEquality(getMethodInstructions(methodPair._1),
        getMethodInstructions(methodPair._2),
        new ComparisonOptions(false, true, false)))
    }
  }

  def getMethodInstructions(method: MetaObject) =
    CodeAnnotation.getCodeInstructions(ByteCodeSkeleton.getMethodAttributes(method)(0))

  def printByteCode(byteCode: MetaObject): String = {
    PrintByteCode.printBytes(getBytes(byteCode))
  }

  def getBytes(byteCode: MetaObject): Seq[Byte] = {
    var output: Seq[Byte] = null
    val transformations: Seq[Injector] = JavaCompiler.byteCodeTransformations ++ Seq(new GetBytes(s => output = s))
    new Transformer(transformations).transform(byteCode)
    output
  }

  def runByteCode(className: String, code: MetaObject, expectedResult: Int) {
    val line = runByteCode(className, code)
    Assert.assertEquals(expectedResult, Integer.parseInt(line))
  }

  def runByteCode(className: String, code: MetaObject) : String = {
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

  def parseAndTransform(className: String, inputDirectory: Path): MetaObject = {
    val input: File = getTestFile(className, inputDirectory)
    compiler.parseAndTransform(input)
  }

  def getTestFile(className: String, inputDirectory: Path): File = {
    val relativeFilePath = inputDirectory / (className + ".java")
    val currentDir = new File(new java.io.File("."))
    val testResources = currentDir / Path("testResources")
    val input: File = File(testResources / relativeFilePath)
    input
  }

  def compileAndRun(className: String, inputDirectory: Path): String = {
    val relativeFilePath = inputDirectory / (className + ".java")
    val currentDir = new File(new java.io.File("."))
    val testOutput = Directory(currentDir / Path("testOutput"))
    val testResources = currentDir / Path("testResources")
    val input: File = File(testResources / relativeFilePath)
    compiler.compile(input, Directory(Path(testOutput.path) / inputDirectory))
    val qualifiedClassName: String = (inputDirectory / Path(className)).segments.reduce[String]((l, r) => l + "." + r)
    TestUtils.runJavaClass(qualifiedClassName, testOutput)
  }

  def compareWithJavacAfterRunning(className: String, inputDirectory: Path) {
    val relativeFilePath = inputDirectory / (className + ".java")
    val currentDir = new File(new java.io.File("."))
    val rootOutput = currentDir / Path("testOutput")
    val actualOutputDirectory = rootOutput / "actual"
    val testResources = currentDir / Path("testResources")
    val input: File = File(testResources / relativeFilePath)

    val expectedOutputDirectory = rootOutput / "expected"
    val javaCompilerOutput = runJavaC(currentDir, input, expectedOutputDirectory)
    Assert.assertEquals("", javaCompilerOutput)

    compiler.compile(input, Directory(actualOutputDirectory / inputDirectory))
    val qualifiedClassName: String = (inputDirectory / Path(className)).segments.reduce[String]((l, r) => l + "." + r)

    val expectedOutput = TestUtils.runJavaClass(qualifiedClassName, expectedOutputDirectory)
    try {
      val actualOutput = TestUtils.runJavaClass(qualifiedClassName, actualOutputDirectory)
      Assert.assertEquals(expectedOutput, actualOutput)
    }
    catch {
      case e: AssertionError =>
        val relativeClassPath = inputDirectory / (className + ".class")
        val actualByteCode = runJavaP((actualOutputDirectory / relativeClassPath).toFile)
        val expectedByteCode = runJavaP((expectedOutputDirectory / relativeClassPath).toFile)

        val originalMessage = e.getMessage
        val errorMessage: String = s"Output comparison failed with message: \n$originalMessage \n\n" +
          s"javac byteCode was: \n\n$expectedByteCode \n\n" +
          s"Actual byteCode was: \n$actualByteCode"
        (rootOutput / "outputLog.txt").createFile().writeAll(errorMessage)
        throw new AssertionError(errorMessage)
    }

  }

  def runJavaP(input: File): String = {
    val processBuilder = Process.apply(s"javap -v $input")
    var line: String = ""
    val logger = ProcessLogger(
      (o: String) => line += o + "\n",
      (e: String) => line += e + "\n")
    val exitValue = processBuilder ! logger
    Assert.assertEquals(line, 0, exitValue)
    line
  }

  def runJavaC(directory: Path, input: File, output: Path): String = {
    val processBuilder = Process.apply(s"javac -d $output $input", directory.jfile)
    var line: String = ""
    val logger = ProcessLogger(
      (o: String) => line += o,
      (e: String) => line += e)
    val exitValue = processBuilder ! logger
    Assert.assertEquals(line, 0, exitValue)
    line
  }


  def runJavaClass(className: String, directory: Path): String = {
    val processBuilder = Process.apply(s"java $className", directory.jfile)
    var line: String = ""
    val logger = ProcessLogger(
      (o: String) => line += o,
      (e: String) => line += e)
    val exitValue = processBuilder ! logger
    Assert.assertEquals(line, 0, exitValue)
    line
  }

  def compareConstantPools(expectedByteCode: MetaObject, compiledCode: MetaObject) {
    val expectedConstantPoolSet = ByteCodeSkeleton.getConstantPool(expectedByteCode)
    val compiledConstantPoolSet = ByteCodeSkeleton.getConstantPool(compiledCode)
    Assert.assertEquals(expectedConstantPoolSet.length, compiledConstantPoolSet.length)
    Assert.assertTrue(expectedConstantPoolSet.forall(expectedItem => {
      val hasEquivalent = compiledConstantPoolSet.exists(compiledItem => MetaObject.deepEquality(compiledItem, expectedItem,
        new ComparisonOptions(false, false, true)))
      hasEquivalent
    }))
  }

  class GetBytes(write: Seq[Byte] => Unit) extends ProgramTransformation {
    override def transform(program: MetaObject, state: TransformationState): Unit = {
      write(PrintByteCode.getBytes(program, state))
    }
  }

}
