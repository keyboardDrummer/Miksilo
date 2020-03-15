package miksilo.modularLanguages.deltas.javac

import miksilo.modularLanguagesutil.JavaLanguageTest

class TestUnqualifiedString extends JavaLanguageTest {

  test("basic") {
    compareWithJavacAfterRunning("UnqualifiedString")
  }
}
