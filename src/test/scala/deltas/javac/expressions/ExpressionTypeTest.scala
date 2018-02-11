package deltas.javac.expressions

import core.smarts.SolveConstraintsDelta
import core.smarts.SolveConstraintsDelta.ConstraintException
import deltas.ClearPhases
import deltas.javac.JavaLanguage
import util.{TestLanguageBuilder, TestUtils}

class ExpressionTypeTest extends TestUtils(TestLanguageBuilder.build(
  Seq(SolveConstraintsDelta,
    ExpressionLanguageDelta,
    ClearPhases) ++
    JavaLanguage.javaSimpleExpression)) {

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
