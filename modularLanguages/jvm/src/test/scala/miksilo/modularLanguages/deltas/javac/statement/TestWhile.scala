package miksilo.modularLanguages.deltas.javac.statement

import miksilo.modularLanguages.util.JavaLanguageTest
import org.scalatest.funsuite.AnyFunSuite

class TestWhile extends AnyFunSuite {

  test("basic") {
    JavaLanguageTest.compareWithJavacAfterRunning("Whilee")
  }
}
