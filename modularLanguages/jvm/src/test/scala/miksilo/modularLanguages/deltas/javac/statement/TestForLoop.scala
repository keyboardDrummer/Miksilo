package miksilo.modularLanguages.deltas.javac.statement

import miksilo.modularLanguages.util.JavaLanguageTest

class TestForLoop extends JavaLanguageTest {

  test("testSimple") {
    compareWithJavacAfterRunning("SimpleForLoop")
  }

  test("WithContinue") {
    compareWithJavacAfterRunning("ForLoopWithContinue")
  }
}
