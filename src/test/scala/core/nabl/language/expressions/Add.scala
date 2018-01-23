package core.nabl.language.expressions

import core.nabl.constraints.ConstraintBuilder
import core.nabl.constraints.scopes.objects.Scope
import core.nabl.constraints.types.objects.{ConcreteType, IntConstraintType, LongConstraintType, Type}
import core.nabl.constraints.types.{CheckSubType, TypesAreEqual}

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
