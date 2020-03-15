package miksilo.modularLanguages.deltas.javac.expressions

import miksilo.modularLanguages.util.JavaLanguageTest

class TestEquality extends JavaLanguageTest {

  test("equality") {
    compareWithJavacAfterRunning("SimpleEquality")
  }
}
