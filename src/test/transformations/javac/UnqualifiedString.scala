package transformations.javac

import org.junit.Test
import util.TestUtils

import scala.reflect.io.Path


class UnqualifiedString {

  @Test
  def test() {
    val inputDirectory = Path("")
    TestUtils.compareWithJavacAfterRunning("UnqualifiedString", inputDirectory)
  }
}
