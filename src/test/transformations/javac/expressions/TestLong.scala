package transformations.javac.expressions

import org.junit.Test
import util.TestUtils

import scala.reflect.io.Path

class TestLong {

  @Test
  def simpleLong() {
    val inputDirectory = Path("")
    TestUtils.compareWithJavacAfterRunning("SimpleLong", inputDirectory)
  }

}
