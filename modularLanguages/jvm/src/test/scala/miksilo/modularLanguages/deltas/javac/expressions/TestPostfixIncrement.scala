package miksilo.modularLanguages.deltas.javac.expressions

import miksilo.modularLanguages.util.JavaLanguageTest
import org.scalatest.funsuite.AnyFunSuite

import scala.reflect.io.Path

class TestPostfixIncrement extends AnyFunSuite {

  test("basic") {
    val inputDirectory = Path("")
    JavaLanguageTest.compareWithJavacAfterRunning("PostFixIncrement", inputDirectory)
  }
}
