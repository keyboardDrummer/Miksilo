package miksilo.modularLanguages.deltas.javac.statement

import util.JavaLanguageTest

class TestJavaGoto extends JavaLanguageTest {

  test("WithContinue") {
    compareWithJavacAfterRunning("ForLoopWithContinue")
  }
}
