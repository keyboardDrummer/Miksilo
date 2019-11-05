package core


import java.io.{BufferedReader, ByteArrayInputStream, InputStream, InputStreamReader}
import java.nio.charset.StandardCharsets
import java.util.stream.Collectors

import scala.reflect.io.{File, Path}

object SourceUtils {

  def getResourceFile(relativeFilePath: Path): InputStream = {
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

  def getResourceFileContents(relativeFilePath: Path): String = {
    val result = new BufferedReader(new InputStreamReader(getResourceFile(relativeFilePath)))
      .lines().collect(Collectors.joining("\n"))
    result.replaceAll("\n", System.lineSeparator())
  }
}
