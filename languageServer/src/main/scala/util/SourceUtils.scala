package util

import java.io._
import java.nio.charset.StandardCharsets
import java.util.stream.Collectors

import core.language.Compilation

import scala.reflect.io.{File, Path}
import scala.sys.process.{Process, ProcessLogger}

object SourceUtils {

  def streamToString(stream: InputStream): String = scala.io.Source.fromInputStream(stream).mkString
  def stringToStream(input: String) = new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8))

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