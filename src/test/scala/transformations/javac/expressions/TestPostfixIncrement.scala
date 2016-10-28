package transformations.javac.expressions

import org.junit.Test
import util.TestUtils

import scala.reflect.io.Path

class TestPostfixIncrement {


  def test() {
    val inputDirectory = Path("")
    TestUtils.compareWithJavacAfterRunning("PostFixIncrement", inputDirectory)
  }
}
