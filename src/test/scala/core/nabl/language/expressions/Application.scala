package core.nabl.language.expressions

import core.nabl.constraints.ConstraintBuilder
import core.nabl.constraints.scopes.objects.Scope
import core.nabl.constraints.types.objects.{FunctionType, Type}

case class Application(function: Expression, value: Expression) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    val functionType = function.getType(builder, scope)
    val argumentType = value.getType(builder, scope)
    builder.typesAreEqual(functionType, FunctionType(argumentType, _type, this))
  }
}
