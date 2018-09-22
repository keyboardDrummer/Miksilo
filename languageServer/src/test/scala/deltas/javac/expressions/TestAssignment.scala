package deltas.javac.expressions

import org.scalatest.FunSuite
import util.LanguageTest

import scala.reflect.io.Path

class TestAssignment extends FunSuite {

  test("Assignment") {
    val inputDirectory = Path("")
    val output: String = LanguageTest.compileAndRun("Assignment", inputDirectory)
    assertResult("1")(output)
  }

  test("AssignmentWithJump") {
    val inputDirectory = Path("")
    val output: String = LanguageTest.compileAndRun("AssignmentWithJump", inputDirectory)
    assertResult("1")(output)
  }
}
