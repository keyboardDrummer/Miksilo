package miksilo.modularLanguages.deltas.javac.expressions

import miksilo.modularLanguages.util.JavaLanguageTest
import org.scalatest.funsuite.AnyFunSuite

import scala.reflect.io.Path

class TestAssignment extends AnyFunSuite {

  test("Assignment") {
    val inputDirectory = Path("")
    val output: String = JavaLanguageTest.compileAndRun("Assignment", inputDirectory)
    assertResult("1")(output)
  }

  test("AssignmentWithJump") {
    val inputDirectory = Path("")
    val output: String = JavaLanguageTest.compileAndRun("AssignmentWithJump", inputDirectory)
    assertResult("1")(output)
  }
}
