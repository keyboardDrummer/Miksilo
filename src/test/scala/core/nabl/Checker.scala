package core.nabl

import core.nabl.language.Program
import core.nabl.language.expressions.Expression
import core.nabl.language.types.{IntType, LanguageType}
import core.nabl.modes.ConstraintClosure
import org.scalatest
import org.scalatest.run

object Checker {

  def failExpression(program: Expression, languageType: LanguageType = IntType): Unit =
  {
    assert(!ConstraintClosure.checkExpression(program, languageType))
  }

  def checkExpression(program: Expression, languageType: LanguageType = IntType): Unit = {
    assert(ConstraintClosure.checkExpression(program, languageType))
  }

  def fail(program: Program): Unit = assert(!ConstraintClosure.check(program))

  def check(program: Program) : Unit = {
    assert(ConstraintClosure.check(program))
  }
}