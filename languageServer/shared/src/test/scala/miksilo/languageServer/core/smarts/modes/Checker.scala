package miksilo.languageServer.core.smarts.modes

import miksilo.languageServer.core.smarts.language.Program
import miksilo.languageServer.core.smarts.language.expressions.Expression
import miksilo.languageServer.core.smarts.language.types.{IntType, LanguageType}

import scala.util.Try

trait Checker
{
  def check(program: Program): Boolean
  def checkExpression(expression: Expression, _type: LanguageType = IntType): Try[Unit]
}
