package miksilo.modularLanguages.deltas.javac.statement

import miksilo.modularLanguages.util.JavaLanguageTest

class TestWhile extends JavaLanguageTest {

  test("basic") {
    compareWithJavacAfterRunning("Whilee")
  }
}
