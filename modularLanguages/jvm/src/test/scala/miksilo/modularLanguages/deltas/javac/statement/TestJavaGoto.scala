package miksilo.modularLanguages.deltas.javac.statement

import miksilo.modularLanguages.util.JavaLanguageTest
import org.scalatest.funsuite.AnyFunSuite

class TestJavaGoto extends AnyFunSuite {

  test("WithContinue") {
    JavaLanguageTest.compareWithJavacAfterRunning("ForLoopWithContinue")
  }
}
