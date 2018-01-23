package core.nabl.modes

import core.nabl.language.Program
import core.nabl.language.expressions.Expression
import core.nabl.language.types.{IntType, LanguageType}

trait Checker
{
  def check(program: Program): Boolean
  def checkExpression(expression: Expression, _type: LanguageType = IntType): Boolean
}
