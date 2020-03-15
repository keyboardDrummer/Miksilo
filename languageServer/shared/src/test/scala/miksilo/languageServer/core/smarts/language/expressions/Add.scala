package miksilo.languageServer.core.smarts.language.expressions

import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.{IntConstraintType, LongConstraintType, Type}
import miksilo.languageServer.core.smarts.types.{CheckSubType, TypesAreEqual}

case class OverloadedAdd(left: Expression, right: Expression) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    builder.add(List(CheckSubType(_type, LongConstraintType)))
    left.constraints(builder, _type, scope)
    right.constraints(builder, _type, scope)
  }
}

case class Add(left: Expression, right: Expression) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    builder.add(List(TypesAreEqual(_type, IntConstraintType)))
    left.constraints(builder, IntConstraintType, scope)
    right.constraints(builder, IntConstraintType, scope)
  }
}
