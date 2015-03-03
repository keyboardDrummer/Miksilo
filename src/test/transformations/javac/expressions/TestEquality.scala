package transformations.javac.expressions

import org.junit.Test
import util.TestUtils

import scala.reflect.io.Path

class TestEquality {

  @Test
  def test() {
    TestUtils.compareWithJavacAfterRunning("SimpleEquality")
  }
}
