package core.smarts.modes

import core.smarts.language.Program
import core.smarts.language.expressions.Expression
import core.smarts.language.types.{IntType, LanguageType}

import scala.util.Try

trait Checker
{
  def check(program: Program): Boolean
  def checkExpression(expression: Expression, _type: LanguageType = IntType): Try[Unit]
}
