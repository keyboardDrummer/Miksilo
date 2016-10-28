package transformations.javac.statement

import org.junit.Test
import util.TestUtils

class TestWhile {


  def testSimple() {
    TestUtils.compareWithJavacAfterRunning("Whilee")
  }
}
