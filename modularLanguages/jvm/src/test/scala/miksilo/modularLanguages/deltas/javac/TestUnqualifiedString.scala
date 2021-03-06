package miksilo.modularLanguages.deltas.javac

import miksilo.modularLanguages.util.JavaLanguageTest
import org.scalatest.funsuite.AnyFunSuite

class TestUnqualifiedString extends AnyFunSuite {

  test("basic") {
    JavaLanguageTest.compareWithJavacAfterRunning("UnqualifiedString")
  }
}
