package miksilo.modularLanguages.deltas.javac.statement

import miksilo.modularLanguages.util.JavaLanguageTest

class TestJavaGoto extends JavaLanguageTest {

  test("WithContinue") {
    compareWithJavacAfterRunning("ForLoopWithContinue")
  }
}
