package transformations.javac.statement

import org.junit.Test
import util.TestUtils

class TestForLoop {

  @Test
  def testSimple() {
    TestUtils.compareWithJavacAfterRunning("SimpleForLoop")
  }

  @Test
  def testWithContinue() {
    TestUtils.compareWithJavacAfterRunning("ForLoopWithContinue")
  }
}
