package deltas.javac.statement

import org.junit.Test
import org.scalatest.FunSuite
import util.LanguageTest

class TestForLoop extends FunSuite {

  test("testSimple") {
    LanguageTest.compareWithJavacAfterRunning("SimpleForLoop")
  }

  test("WithContinue") {
    LanguageTest.compareWithJavacAfterRunning("ForLoopWithContinue")
  }
}
