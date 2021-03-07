package miksilo.modularLanguages.deltas.javac.expressions

import miksilo.modularLanguages.core.SolveConstraintsDelta
import miksilo.modularLanguages.deltas.ClearPhases
import miksilo.modularLanguages.deltas.expression.ExpressionLanguageDelta
import miksilo.modularLanguages.deltas.javac.JavaToByteCodeLanguage
import miksilo.modularLanguages.util.TestLanguageBuilder
import miksilo.modularLanguages.util.LanguageTest
import org.scalatest.funsuite.AnyFunSuite

class ExpressionTypeTest extends AnyFunSuite {

  val utils = new LanguageTest(TestLanguageBuilder.buildWithParser(
    Seq(SolveConstraintsDelta,
      ExpressionLanguageDelta,
      ClearPhases) ++
      JavaToByteCodeLanguage.javaSimpleExpression))

  test("int + int") {
    val program = "3 + 2"
    val compilation = utils.compile(program)
    assert(compilation.remainingConstraints.isEmpty)
  }

  test("int + long") {
    val program = "3 + 2l"
    val compilation = utils.compile(program)
    assert(compilation.remainingConstraints.nonEmpty)
  }

  test("long + long") {
    val program = "3l + 2l"
    val compilation = utils.compile(program)
    assert(compilation.remainingConstraints.isEmpty)
  }
}
