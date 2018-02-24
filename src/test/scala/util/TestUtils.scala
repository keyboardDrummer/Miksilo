package util

import java.io.{BufferedInputStream, ByteArrayInputStream, InputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.Files

import application.compilerCockpit.{MarkOutputGrammar, PrettyPrint}
import core.language.node.{ComparisonOptions, Node}
import core.language.Compilation
import deltas.bytecode.ByteCodeMethodInfo.MethodInfo
import deltas.bytecode.ByteCodeSkeleton.ClassFile
import deltas.bytecode.PrintByteCode
import deltas.javac.JavaLanguage
import org.scalatest.FunSuite
import util.SourceUtils.LineProcessLogger

import scala.reflect.io.{Directory, File, Path}
import scala.sys.process.Process

object TestUtils extends TestUtils(TestLanguageBuilder.build(JavaLanguage.javaCompilerDeltas)) {
}

class TestUtils(val language: TestingLanguage) extends FunSuite {

  def toFile(fileName: String, program: String): Path = {
    val directory = Directory.makeTemp()
    val file = File(directory / fileName).addExtension("java")
    file.createFile()
    file.writeAll(program)
    file
  }

  def currentDir: Directory = Directory.Current.get
  def rootOutput: Directory = Directory(currentDir / "testOutput")
  def actualOutputDirectory: Directory = Directory(rootOutput / "actual")
  def expectedOutputDirectory: Directory = Directory(rootOutput / "expected")
  actualOutputDirectory.createDirectory(force = true)
  expectedOutputDirectory.createDirectory(force = true)

  def testInstructionEquivalence(expectedByteCode: ClassFile[Node], compiledCode: ClassFile[Node]) {
    for (methodPair <- expectedByteCode.methods.zip(compiledCode.methods)) {
      assert(ComparisonOptions(compareIntegers = false, takeAllRightKeys = false).deepEquality(getMethodInstructions(methodPair._1), getMethodInstructions(methodPair._2)))
    }
  }

  def getMethodInstructions(method: MethodInfo[Node]): Seq[Node] = method.codeAttribute.instructions

  def printByteCode(byteCode: Node): String = {
    PrintByteCode.printBytes(SourceUtils.getBytes(byteCode))
  }

  def runByteCode(className: String, code: Node, expectedResult: Int) {
    val line = SourceUtils.runByteCode(className, code)
    assertResult(expectedResult)(Integer.parseInt(line))
  }

  def parseAndTransform(className: String, inputDirectory: Path): Node = {
    val input: String = SourceUtils.getJavaTestFileContents(className, inputDirectory)
    language.parseAndTransform(SourceUtils.stringToStream(input)).program
  }

    def compileAndRun(fileName: String, inputDirectory: Path = Path("")): String = {
    val className: String = SourceUtils.fileNameToClassName(fileName)
    val relativeFilePath = inputDirectory / (className + ".java")
    val input: InputStream = SourceUtils.getTestFile(relativeFilePath)
    val outputDirectory = actualOutputDirectory / inputDirectory
    outputDirectory.createDirectory(force = true)
    val outputFile = outputDirectory / className addExtension "class"
    language.compile(input, outputFile.toFile)
    val qualifiedClassName: String = (inputDirectory / className).segments.reduce[String]((l, r) => l + "." + r)
    SourceUtils.runJavaClass(qualifiedClassName, actualOutputDirectory)
  }

  def compile(input: String): Unit = {
    language.parseAndTransform(input)
  }

  def compileAndPrettyPrint(input: String): String = {
    compileAndPrettyPrint(new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8)))
  }

  def compileAndPrettyPrint(input: InputStream): String = {

    val prettyPrint = PrettyPrint(recover = true)
    val splicedDeltas = language.replace(MarkOutputGrammar,Seq(prettyPrint))
    val newCompiler = TestLanguageBuilder.build(splicedDeltas)

    val state = newCompiler.parseAndTransform(input)
    state.output
  }

  def compareWithJavacAfterRunning(className: String, input: String): Unit = {
    val tempDirectory = Directory.makeTemp()
    tempDirectory.createDirectory()
    val file = (tempDirectory / className).toFile.addExtension("java")
    file.writeAll(input)
    compareWithJavacAfterRunning(file)
  }

  def compareWithJavacAfterRunning(name: String, inputDirectory: Path = Path("")): Unit = {
    compareWithJavacAfterRunning(File(inputDirectory / name))
  }

  def compareWithJavacAfterRunning(inputFile: Path) {
    val className = inputFile.stripExtension

    val relativeFilePath = inputFile.changeExtension("java")
    val input: BufferedInputStream = new BufferedInputStream(SourceUtils.getTestFile(relativeFilePath))
    input.mark(Integer.MAX_VALUE)

    val javaCompilerOutput = TestLanguageBuilder.profile("javac", runJavaCIfNeeded(className, input, expectedOutputDirectory))
    input.reset()
    assertResult("")(javaCompilerOutput)

    val outputFile = File((actualOutputDirectory / className).addExtension("class"))
    val state = profile("Miksilo compile", language.compile(input, outputFile))
    val qualifiedClassName: String = (inputFile.parent / className).segments.reduce[String]((l, r) => l + "." + r)

    val expectedOutput = profile("Java run expected", SourceUtils.runJavaClass(qualifiedClassName, expectedOutputDirectory))
    try {
      val actualOutput = profile("Java run actual", SourceUtils.runJavaClass(qualifiedClassName, actualOutputDirectory))
      assertResult(expectedOutput)(actualOutput)
    }
    catch {
      case e: AssertionError =>
        throw new AssertionError(getErrorMessage(inputFile, expectedOutputDirectory, state, e))
    }
  }

  def profile[T](description: String, action: => T): T = TestLanguageBuilder.profile(description, action)


  def getErrorMessage(inputFile: Path, expectedOutputDirectory: Path, state: Compilation, e: AssertionError): String = {
    val relativeClassPath = inputFile.changeExtension(".class")
    val actualByteCodeAccordingToJavap = runJavaP((actualOutputDirectory / relativeClassPath).toFile)
    val expectedByteCodeAccordingToJavap = runJavaP((expectedOutputDirectory / relativeClassPath).toFile)

    val prettyPrintByteCodeCompiler = TestLanguageBuilder.build(Seq(new PrettyPrint) ++ JavaLanguage.byteCodeDeltas)
    val output = prettyPrintByteCodeCompiler.transform(state.program).output
    val prettyPrintActualByteCode = output

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
    val logger = new LineProcessLogger()
    val exitValue = processBuilder ! logger
    assertResult(0, logger.line)(exitValue)
    logger.line
  }

  def runJavaCIfNeeded(name: String, inputStream: InputStream, output: Directory): String = {
    val outputFileName = name + ".class"
    if (output.files.exists(f => f.name == outputFileName))
      return ""

    runJavaC(name, inputStream, output)
  }

  private def runJavaC(name: String, inputStream: InputStream, output: Directory) = {
    val path = Files.createTempDirectory("input")
    val fileName = name + ".java"
    val inputFile = path.resolve(fileName)
    Files.copy(inputStream, inputFile)
    val processBuilder = Process.apply(s"javac -d $output ${inputFile.toString}")
    val logger = new LineProcessLogger()
    val exitValue = processBuilder ! logger
    assertResult(0, s"Java compiler did not exit successfully.\nMessage was ${logger.line}")(exitValue)
    logger.line
  }

  def compareConstantPools(expectedByteCode: Node, compiledCode: Node) {
    val expectedConstantPoolSet = expectedByteCode.constantPool.constants
    val compiledConstantPoolSet = compiledCode.constantPool.constants
    assertResult(expectedConstantPoolSet.length)(compiledConstantPoolSet.length)
    assert(expectedConstantPoolSet.forall(expectedItem => {
      val hasEquivalent = compiledConstantPoolSet.exists(compiledItem =>
        ComparisonOptions(compareIntegers = false, takeAllLeftKeys = false).deepEquality(compiledItem, expectedItem))
      hasEquivalent
    }), s"$expectedConstantPoolSet was not $compiledConstantPoolSet")
  }

}
