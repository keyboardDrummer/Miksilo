package core.smarts.language.expressions

import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type

case class Variable(name: String) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    builder.resolve(name, this, scope, Some(_type))
  }

  override def toString = s"Variable($name)"
}
