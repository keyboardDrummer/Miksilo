package core

import java.io.{BufferedReader, ByteArrayInputStream, InputStream, InputStreamReader}
import java.nio.charset.StandardCharsets
import java.util.stream.Collectors

import scala.reflect.io.{File, Path}

object TestUtils {

  def runPerformanceTest(targetTime: Double, maxRepetitions: Integer, action: () => Unit): Unit = {
    var repetition = 0
    var success = false
    var bestTime = Double.MaxValue
    while(!success && repetition < maxRepetitions) {
      val before = System.currentTimeMillis()
      action()
      val after = System.currentTimeMillis()
      val newTime = after - before
      bestTime = Math.min(bestTime, newTime)
      if (bestTime <= targetTime) {
        success = true
      }
      repetition += 1
    }
    System.out.println(s"time: $bestTime")
    System.out.println(s"repetitions: $repetition")
    assert(success)
  }

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
