package deltas.javac.expressions

import core.smarts.SolveConstraintsDelta
import core.smarts.SolveConstraintsDelta.ConstraintException
import deltas.ClearPhases
import deltas.javac.JavaCompilerDeltas
import util.{TestLanguageBuilder, TestUtils}

class ExpressionTypeTest extends TestUtils(TestLanguageBuilder.build(
  Seq(SolveConstraintsDelta,
    ExpressionLanguageDelta,
    ClearPhases) ++
    JavaCompilerDeltas.javaSimpleExpression)) {

  test("int + int") {
    val program = "3 + 2"
    compile(program)
  }

  test("int + long") {
    val program = "3 + 2l"
    assertThrows[ConstraintException](compile(program))
  }

  test("long + long") {
    val program = "3l + 2l"
    compile(program)
  }
}
