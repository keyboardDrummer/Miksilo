package util

import core.particles._
import core.particles.node.Node
import transformations.bytecode.PrintByteCode
import transformations.javac.JavaCompiler

import scala.reflect.io.{File, Path}
import scala.sys.process.{Process, ProcessLogger}

object SourceUtils {

  def getJavaTestFile(fileName: String, inputDirectory: Path = Path("")): File = {
    val className = SourceUtils.fileNameToClassName(fileName)
    val relativeFilePath = inputDirectory / (className + ".java")
    SourceUtils.getTestFile(relativeFilePath)
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

  def getBytes(byteCode: Node): Seq[Byte] = {
    var output: Seq[Byte] = null
    val particles: Seq[Delta] = Seq(new GetBytes(s => output = s)) ++ JavaCompiler.byteCodeTransformations
    CompilerBuilder.build(particles).transform(byteCode)
    output
  }

  class GetBytes(write: Seq[Byte] => Unit) extends DeltaWithPhase {
    override def transform(program: Node, state: Compilation): Unit = {
      write(PrintByteCode.getBytes(program, state))
    }

    override def description: String = "Writes the current program as JVM class file bytes to a function."
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

  def fileNameToClassName(fileName: String): String = {
    if (fileName.endsWith(".java")) fileName.dropRight(5) else fileName
  }

  def getJavaTestFileContents(fileName: String, inputDirectory: Path = Path("")): String = {
    val className = fileNameToClassName(fileName)
    val relativeFilePath = inputDirectory / (className + ".java")
    getTestFileContents(relativeFilePath)
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

  def getTestFileContents(relativeFilePath: Path): String = {
    getTestFile(relativeFilePath).slurp().replaceAll("\r\n","\n").replaceAll("\n", System.lineSeparator())
  }
}