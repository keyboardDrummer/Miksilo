package miksilo.modularLanguages.deltas.javac.statement

import miksilo.modularLanguages.util.JavaLanguageTest
import org.scalatest.funsuite.AnyFunSuite

class TestForLoop extends AnyFunSuite {

  test("testSimple") {
    JavaLanguageTest.compareWithJavacAfterRunning("SimpleForLoop")
  }

  test("WithContinue") {
    JavaLanguageTest.compareWithJavacAfterRunning("ForLoopWithContinue")
  }
}
