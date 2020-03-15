package miksilo.modularLanguages.deltas.javac.expressions

import miksilo.modularLanguages.util.JavaLanguageTest

import scala.reflect.io.Path

class TestAssignment extends JavaLanguageTest {

  test("Assignment") {
    val inputDirectory = Path("")
    val output: String = compileAndRun("Assignment", inputDirectory)
    assertResult("1")(output)
  }

  test("AssignmentWithJump") {
    val inputDirectory = Path("")
    val output: String = compileAndRun("AssignmentWithJump", inputDirectory)
    assertResult("1")(output)
  }
}
