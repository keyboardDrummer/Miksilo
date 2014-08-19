package transformations.javac.expressions

import org.junit.Test
import transformations.javac.TestUtils

import scala.reflect.io.Path

class TestBooleans {

  @Test
  def test() {
    val inputDirectory = Path("")
    TestUtils.compareWithJavacAfterRunning("Booleans", inputDirectory)
  }
}
