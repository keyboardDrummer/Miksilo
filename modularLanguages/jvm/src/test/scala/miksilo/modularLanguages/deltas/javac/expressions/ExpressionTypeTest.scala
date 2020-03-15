package miksilo.modularLanguages.deltas.javac.expressions

import core.SolveConstraintsDelta
import miksilo.modularLanguages.deltas.ClearPhases
import miksilo.modularLanguages.deltas.expression.ExpressionLanguageDelta
import miksilo.modularLanguages.deltas.javac.JavaToByteCodeLanguage
import util.{LanguageTest, TestLanguageBuilder}

class ExpressionTypeTest extends LanguageTest(TestLanguageBuilder.buildWithParser(
  Seq(SolveConstraintsDelta,
    ExpressionLanguageDelta,
    ClearPhases) ++
    JavaToByteCodeLanguage.javaSimpleExpression)) {

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
