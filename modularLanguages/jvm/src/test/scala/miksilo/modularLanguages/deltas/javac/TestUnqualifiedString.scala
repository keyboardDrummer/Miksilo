package miksilo.modularLanguages.deltas.javac

import miksilo.modularLanguages.util.JavaLanguageTest

class TestUnqualifiedString extends JavaLanguageTest {

  test("basic") {
    compareWithJavacAfterRunning("UnqualifiedString")
  }
}
