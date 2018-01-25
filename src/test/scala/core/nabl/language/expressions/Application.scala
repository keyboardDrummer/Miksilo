package core.nabl.language.expressions

import core.nabl.ConstraintBuilder
import core.nabl.scopes.objects.Scope
import core.nabl.types.objects.{FunctionType, Type}

case class Application(function: Expression, value: Expression) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    val functionType = function.getType(builder, scope)
    val argumentType = value.getType(builder, scope)
    builder.typesAreEqual(functionType, FunctionType(argumentType, _type, this))
  }
}
