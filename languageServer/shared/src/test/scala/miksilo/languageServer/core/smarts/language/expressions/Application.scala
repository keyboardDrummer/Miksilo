package miksilo.languageServer.core.smarts.language.expressions

import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.{FunctionType, Type}

case class Application(function: Expression, value: Expression) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    val functionType = function.getType(builder, scope)
    val argumentType = value.getType(builder, scope)
    builder.typesAreEqual(functionType, FunctionType(argumentType, _type, Some(this)))
  }
}
