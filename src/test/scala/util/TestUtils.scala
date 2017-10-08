package util

import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.StandardCharsets

import application.compilerCockpit.{MarkOutputGrammar, PrettyPrint}
import core.particles.Compilation
import core.particles.node.{ComparisonOptions, Node}
import org.scalatest.FunSuite
import transformations.bytecode.ByteCodeMethodInfo.ByteCodeMethodInfoWrapper
import transformations.bytecode.ByteCodeSkeleton.ByteCodeWrapper
import transformations.bytecode.PrintByteCode
import transformations.javac.JavaCompilerDeltas
import util.SourceUtils.LineProcessLogger

import scala.reflect.io.{Directory, File, Path}
import scala.sys.process.Process

object TestUtils extends TestUtils(CompilerBuilder.build(JavaCompilerDeltas.javaCompilerTransformations)) {
}

class TestUtils(val compiler: TestingCompiler) extends FunSuite {

  def toFile(program: String): String = {
    val fileName = File.makeTemp(suffix = ".java")
    val file = fileName.createFile()
    file.writeAll(program)
    fileName.toString()
  }

  def currentDir = new File(new java.io.File("."))
  def rootOutput: Path = currentDir / Path("testOutput")
  def actualOutputDirectory: Path = rootOutput / "actual"

  def testInstructionEquivalence(expectedByteCode: ByteCodeWrapper[Node], compiledCode: ByteCodeWrapper[Node]) {
    for (methodPair <- expectedByteCode.methods.zip(compiledCode.methods)) {
      assert(ComparisonOptions(compareIntegers = false, takeAllRightKeys = false).deepEquality(getMethodInstructions(methodPair._1), getMethodInstructions(methodPair._2)))
    }
  }

  def getMethodInstructions(method: ByteCodeMethodInfoWrapper[Node]): Seq[Node] = method.codeAttribute.instructions

  def printByteCode(byteCode: Node): String = {
    PrintByteCode.printBytes(SourceUtils.getBytes(byteCode))
  }

  def runByteCode(className: String, code: Node, expectedResult: Int) {
    val line = SourceUtils.runByteCode(className, code)
    assertResult(expectedResult)(Integer.parseInt(line))
  }

  def stringToInputStream(input: String) = new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8))

  def parseAndTransform(className: String, inputDirectory: Path): Node = {
    val input: String = SourceUtils.getJavaTestFileContents(className, inputDirectory)
    compiler.parseAndTransform(stringToInputStream(input)).program
  }

  def compileAndRun(fileName: String, inputDirectory: Path = Path("")): String = {
    val className: String = SourceUtils.fileNameToClassName(fileName)
    val relativeFilePath = inputDirectory / (className + ".java")
    val currentDir = new File(new java.io.File("."))
    val testOutput = Directory(currentDir / Path("testOutput"))
    val input: File = SourceUtils.getTestFile(relativeFilePath)
    compiler.compile(input, Directory(Path(testOutput.path) / inputDirectory))
    val qualifiedClassName: String = (inputDirectory / className).segments.reduce[String]((l, r) => l + "." + r)
    SourceUtils.runJavaClass(qualifiedClassName, testOutput)
  }

  def compileAndPrettyPrint(input: String): String = {
    compileAndPrettyPrint(new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8)))
  }

  def compileAndPrettyPrint(input: InputStream): String = {

    val prettyPrint = PrettyPrint(recover = true)
    val splicedParticles = compiler.replace(MarkOutputGrammar,Seq(prettyPrint))
    val newCompiler = CompilerBuilder.build(splicedParticles)

    val state = newCompiler.parseAndTransform(input)
    state.output
  }

  def compareWithJavacAfterRunning(fileName: String, inputDirectory: Path = Path("")) {
    val className = SourceUtils.fileNameToClassName(fileName)

    val relativeFilePath = inputDirectory / (className + ".java")
    val input: File = SourceUtils.getTestFile(relativeFilePath)

    val expectedOutputDirectory = rootOutput / "expected"
    expectedOutputDirectory.createDirectory()
    val javaCompilerOutput = CompilerBuilder.profile("javac", runJavaC(currentDir, input, expectedOutputDirectory))
    assertResult("")(javaCompilerOutput)

    val state = profile("blender compile", compiler.compile(input, Directory(actualOutputDirectory / inputDirectory)))
    val qualifiedClassName: String = (inputDirectory / Path(className)).segments.reduce[String]((l, r) => l + "." + r)

    val expectedOutput = profile("Java run expected", SourceUtils.runJavaClass(qualifiedClassName, expectedOutputDirectory))
    try {
      val actualOutput = profile("Java run actual", SourceUtils.runJavaClass(qualifiedClassName, actualOutputDirectory))
      assertResult(expectedOutput)(actualOutput)
    }
    catch {
      case e: AssertionError =>
        throw new AssertionError(getErrorMessage(className, inputDirectory, expectedOutputDirectory, state, e))
    }
  }

  def profile[T](description: String, action: => T): T = CompilerBuilder.profile(description, action)


  def getErrorMessage(className: String, inputDirectory: Path, expectedOutputDirectory: Path, state: Compilation, e: AssertionError): String = {
    val relativeClassPath = inputDirectory / (className + ".class")
    val actualByteCodeAccordingToJavap = runJavaP((actualOutputDirectory / relativeClassPath).toFile)
    val expectedByteCodeAccordingToJavap = runJavaP((expectedOutputDirectory / relativeClassPath).toFile)

    val prettyPrintByteCodeCompiler = CompilerBuilder.build(Seq(new PrettyPrint) ++ JavaCompilerDeltas.byteCodeTransformations)
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

  def runJavaC(directory: Path, input: File, output: Path): String = {
    val processBuilder = Process.apply(s"javac -d $output $input", directory.jfile)
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
