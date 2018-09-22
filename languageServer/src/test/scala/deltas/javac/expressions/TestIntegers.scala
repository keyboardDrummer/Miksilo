package deltas.javac.expressions

import org.junit.Test
import org.scalatest.FunSuite
import util.LanguageTest

class TestIntegers extends FunSuite {

  test("simpleInteger") {
    LanguageTest.compareWithJavacAfterRunning("BigInteger")
  }
}
