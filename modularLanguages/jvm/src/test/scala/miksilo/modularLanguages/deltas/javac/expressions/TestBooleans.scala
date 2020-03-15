package miksilo.modularLanguages.deltas.javac.expressions

import util.JavaLanguageTest

class TestBooleans extends JavaLanguageTest {

  def test(): Unit = {
    compareWithJavacAfterRunning("Booleans")
  }
}
