package core.nabl.language.expressions

import core.nabl.constraints.ConstraintBuilder
import core.nabl.constraints.scopes.objects.Scope
import core.nabl.constraints.types.objects._

case class LongConst(value: Long) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = builder.typesAreEqual(_type, LongConstraintType)
}

case class Const(value: Int) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = builder.typesAreEqual(_type, IntConstraintType)
}

case class BoolConst(value: Boolean) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = builder.typesAreEqual(_type, BoolConstraintType)
}
