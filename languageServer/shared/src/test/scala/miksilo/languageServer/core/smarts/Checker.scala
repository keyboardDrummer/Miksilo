package miksilo.languageServer.core.smarts

import miksilo.languageServer.core.smarts.language.Program
import miksilo.languageServer.core.smarts.language.expressions.Expression
import miksilo.languageServer.core.smarts.language.types.{IntType, LanguageType}
import miksilo.languageServer.core.smarts.modes.ConstraintClosure

import scala.util.Failure

object Checker {

  def failExpression(program: Expression, languageType: LanguageType = IntType): Unit =
  {
    assert(ConstraintClosure.checkExpression(program, languageType).isFailure)
  }

  def checkExpression(program: Expression, languageType: LanguageType = IntType): Unit = {
    val result = ConstraintClosure.checkExpression(program, languageType)
    assert(result.isSuccess, result.asInstanceOf[Failure[Unit]].exception.toString)
  }

  def fail(program: Program): Unit = assert(!ConstraintClosure.check(program))

  def check(program: Program) : Unit = {
    assert(ConstraintClosure.check(program))
  }
}