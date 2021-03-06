package miksilo.modularLanguages.deltas.javac.expressions

import miksilo.modularLanguages.util.JavaLanguageTest
import org.scalatest.funsuite.AnyFunSuite

class TestEquality extends AnyFunSuite {

  test("equality") {
    JavaLanguageTest.compareWithJavacAfterRunning("SimpleEquality")
  }
}
