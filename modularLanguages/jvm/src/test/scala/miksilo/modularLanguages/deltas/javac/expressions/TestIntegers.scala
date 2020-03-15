package miksilo.modularLanguages.deltas.javac.expressions

import miksilo.modularLanguages.util.JavaLanguageTest

class TestIntegers extends JavaLanguageTest {

  test("simpleInteger") {
    compareWithJavacAfterRunning("BigInteger")
  }
}
