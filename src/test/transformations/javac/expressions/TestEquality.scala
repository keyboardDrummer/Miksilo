package transformations.javac.expressions

import org.junit.Test
import util.TestUtils

class TestEquality {

  @Test
  def test() {
    TestUtils.compareWithJavacAfterRunning("SimpleEquality")
  }
}
