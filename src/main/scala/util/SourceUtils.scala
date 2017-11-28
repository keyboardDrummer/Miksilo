package util

import core.deltas._
import core.deltas.node.Node
import deltas.bytecode.PrintByteCode
import deltas.javac.JavaCompilerDeltas

import scala.reflect.io.{File, Path}
import scala.sys.process.{Process, ProcessLogger}

object SourceUtils {

  def getJavaTestFile(fileName: String, inputDirectory: Path = Path("")): File = {
    val className = SourceUtils.fileNameToClassName(fileName)
    val relativeFilePath = inputDirectory / (className + ".java")
    SourceUtils.getTestFile(relativeFilePath)
  }

  class LineProcessLogger extends ProcessLogger {
    var line = ""

    /** Will be called with each line read from the process output stream.
      */
    def out(s: => String): Unit = {
      if (!s.contains("Picked up _JAVA_OPTIONS"))
        line += s
    }

    /** Will be called with each line read from the process error stream.
      */
    def err(s: => String): Unit = {
      if (!s.contains("Picked up _JAVA_OPTIONS"))
        line += s
    }

    def buffer[T](f: => T): T = f
  }

  def runJavaClass(className: String, directory: Path): String = {
    val processBuilder = Process.apply(s"java $className", directory.jfile)
    val logger = new LineProcessLogger()
    processBuilder ! logger
    logger.line
  }

  def getBytes(byteCode: Node): Seq[Byte] = {
    var output: Seq[Byte] = null
    val particles: Seq[Delta] = Seq(new GetBytes(s => output = s)) ++ JavaCompilerDeltas.byteCodeDeltas
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