package deltas.javac.statement

import org.scalatest.FunSuite
import util.LanguageTest

class TestJavaGoto extends FunSuite {

  test("WithContinue") {
    LanguageTest.compareWithJavacAfterRunning("ForLoopWithContinue")
  }
}
