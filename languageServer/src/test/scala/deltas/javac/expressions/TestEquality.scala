package deltas.javac.expressions

import org.junit.Test
import org.scalatest.FunSuite
import util.LanguageTest

class TestEquality extends FunSuite {

  test("equality") {
    LanguageTest.compareWithJavacAfterRunning("SimpleEquality")
  }
}
