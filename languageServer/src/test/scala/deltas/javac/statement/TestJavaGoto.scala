package deltas.javac.statement

import org.scalatest.FunSuite
import util.TestUtils

class TestJavaGoto extends FunSuite {

  test("WithContinue") {
    TestUtils.compareWithJavacAfterRunning("ForLoopWithContinue")
  }
}
