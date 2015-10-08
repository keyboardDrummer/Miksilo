package transformations.javac.expressions

import org.junit.Test
import util.TestUtils

class TestBooleans {

  @Test
  def test() {
    TestUtils.compareWithJavacAfterRunning("Booleans")
  }
}
