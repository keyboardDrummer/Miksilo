package miksilo.modularLanguages.deltas.javac.expressions

import util.JavaLanguageTest

class TestIntegers extends JavaLanguageTest {

  test("simpleInteger") {
    compareWithJavacAfterRunning("BigInteger")
  }
}
