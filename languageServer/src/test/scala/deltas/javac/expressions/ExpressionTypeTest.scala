package deltas.javac.expressions

import core.smarts.SolveConstraintsDelta
import deltas.ClearPhases
import deltas.javac.JavaLanguage
import util.{TestLanguageBuilder, LanguageTest}

class ExpressionTypeTest extends LanguageTest(TestLanguageBuilder.build(
  Seq(SolveConstraintsDelta,
    ExpressionLanguageDelta,
    ClearPhases) ++
    JavaLanguage.javaSimpleExpression)) {

  test("int + int") {
    val program = "3 + 2"
    val compilation = compile(program)
    assert(compilation.remainingConstraints.isEmpty)
  }

  test("int + long") {
    val program = "3 + 2l"
    val compilation = compile(program)
    assert(compilation.remainingConstraints.nonEmpty)
  }

  test("long + long") {
    val program = "3l + 2l"
    val compilation = compile(program)
    assert(compilation.remainingConstraints.isEmpty)
  }
}
