package miksilo.languageServer.core.smarts.language.expressions

import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.Type

case class Variable(name: String) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    builder.resolve(name, scope, this, Some(_type))
  }

  override def toString = s"Variable($name)"
}
