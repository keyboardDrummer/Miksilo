package miksilo.modularLanguages.deltas.javac.expressions

import miksilo.modularLanguages.util.JavaLanguageTest

class TestBooleans extends JavaLanguageTest {

  def test(): Unit = {
    compareWithJavacAfterRunning("Booleans")
  }
}
