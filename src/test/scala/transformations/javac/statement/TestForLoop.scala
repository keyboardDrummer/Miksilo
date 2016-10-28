package transformations.javac.statement

import org.junit.Test
import util.TestUtils



class TestForLoop {


  def testSimple() {
    TestUtils.compareWithJavacAfterRunning("SimpleForLoop")
  }


  def testWithContinue() {
    TestUtils.compareWithJavacAfterRunning("ForLoopWithContinue")
  }
}
