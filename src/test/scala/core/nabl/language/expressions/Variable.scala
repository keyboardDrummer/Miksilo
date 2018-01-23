package core.nabl.language.expressions

import core.nabl.constraints._
import core.nabl.constraints.scopes.objects.Scope
import core.nabl.constraints.types.objects.Type

case class Variable(name: String) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    builder.resolve(name, this, scope, Some(_type))
  }

  override def toString = s"Variable($name)"
}
