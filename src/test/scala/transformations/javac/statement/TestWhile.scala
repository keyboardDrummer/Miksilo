package transformations.javac.statement

import org.junit.Test
import util.TestUtils

class TestWhile {

  @Test
  def testSimple() {
    TestUtils.compareWithJavacAfterRunning("Whilee")
  }
}
