package miksilo.modularLanguages.deltas.javac.expressions

import miksilo.modularLanguages.util.JavaLanguageTest
import org.scalatest.funsuite.AnyFunSuite

class TestIntegers extends AnyFunSuite {

  test("simpleInteger") {
    JavaLanguageTest.compareWithJavacAfterRunning("BigInteger")
  }
}
