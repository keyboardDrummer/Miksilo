package deltas.javac.statement

import org.junit.Test
import org.scalatest.FunSuite
import util.TestUtils

class TestForLoop extends FunSuite {

  test("testSimple") {
    TestUtils.compareWithJavacAfterRunning("SimpleForLoop")
  }

  test("WithContinue") {
    TestUtils.compareWithJavacAfterRunning("ForLoopWithContinue")
  }
}
