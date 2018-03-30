package util

import java.io._
import java.nio.charset.StandardCharsets
import java.util.stream.Collectors

import core.deltas._
import core.language.node.Node
import core.language.Compilation
import deltas.bytecode.PrintByteCode
import deltas.javac.JavaLanguage

import scala.reflect.io.{File, Path}
import scala.sys.process.{Process, ProcessLogger}

object SourceUtils {

  def streamToString(stream: InputStream): String = scala.io.Source.fromInputStream(stream).mkString
  def stringToStream(input: String) = new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8))

  def getJavaTestFile(fileName: String, inputDirectory: Path = Path("")): InputStream = {
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
    val particles: Seq[Delta] = Seq(new GetBytes(s => output = s)) ++ JavaLanguage.byteCodeDeltas
    TestLanguageBuilder.build(particles).transform(byteCode)
    output
  }

  class GetBytes(write: Seq[Byte] => Unit) extends DeltaWithPhase {
    override def transformProgram(program: Node, compilation: Compilation): Unit = {
      write(PrintByteCode.getBytes(compilation, program))
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

  def fileNameToClassName(fileName: Path): String = {
    val name = fileName.name
    if (name.endsWith(".java")) name.dropRight(5) else name
  }

  def getJavaTestFileContents(fileName: String, inputDirectory: Path = Path("")): String = {
    val className = fileNameToClassName(fileName)
    val relativeFilePath = inputDirectory / (className + ".java")
    getTestFileContents(relativeFilePath)
  }

  def getTestFile(relativeFilePath: Path): InputStream = {
    if (relativeFilePath.isAbsolute)
      return File(relativeFilePath).inputStream()

    val fullPath = relativeFilePath
    var testResources: InputStream = ClassLoader.getSystemResourceAsStream("/" + fullPath.path)

    if (testResources == null)
      testResources = getClass.getResourceAsStream("/" + fullPath.path)

    if (testResources == null)
      throw new RuntimeException(s"Test file ${fullPath.path} not found")
    testResources
  }

  def getTestFileContents(relativeFilePath: Path): String = {
    val result = new BufferedReader(new InputStreamReader(getTestFile(relativeFilePath)))
      .lines().collect(Collectors.joining("\n"))
    result.replaceAll("\n", System.lineSeparator())
  }
}