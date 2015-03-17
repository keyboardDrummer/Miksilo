package transformations.javac.statement

import org.junit.Test
import util.TestUtils

class TestSimpleForLoop {

  @Test
  def test() {
    TestUtils.compareWithJavacAfterRunning("SimpleForLoop")
  }
}
