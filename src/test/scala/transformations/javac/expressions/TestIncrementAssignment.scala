package transformations.javac.expressions

import org.junit.Test
import util.TestUtils

import scala.reflect.io.Path

class TestIncrementAssignment {


  def test() {
    val inputDirectory = Path("")
    TestUtils.compareWithJavacAfterRunning("IncrementAssignment", inputDirectory)
  }
}
