package miksilo.modularLanguages.deltas.javac.expressions

import util.JavaLanguageTest

class TestEquality extends JavaLanguageTest {

  test("equality") {
    compareWithJavacAfterRunning("SimpleEquality")
  }
}
