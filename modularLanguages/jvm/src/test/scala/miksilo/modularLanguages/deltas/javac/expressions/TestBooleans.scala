package miksilo.modularLanguages.deltas.javac.expressions

import miksilo.modularLanguages.util.JavaLanguageTest
import org.scalatest.funsuite.AnyFunSuite

class TestBooleans extends AnyFunSuite {

  def test(): Unit = {
    JavaLanguageTest.compareWithJavacAfterRunning("Booleans")
  }
}
