package miksilo.modularLanguages.deltas.javac.expressions

import miksilo.modularLanguages.util.JavaLanguageTest
import org.scalatest.funsuite.AnyFunSuite

import scala.reflect.io.Path

class TestIncrementAssignment extends AnyFunSuite {

  test("incrementAssignment") {
    val inputDirectory = Path("")
    JavaLanguageTest.compareWithJavacAfterRunning("IncrementAssignment", inputDirectory)
  }
}
