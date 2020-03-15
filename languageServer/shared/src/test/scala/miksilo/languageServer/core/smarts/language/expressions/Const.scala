package miksilo.languageServer.core.smarts.language.expressions

import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects._

case class LongConst(value: Long) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = builder.typesAreEqual(_type, LongConstraintType)
}

case class Const(value: Int) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = builder.typesAreEqual(_type, IntConstraintType)
}

case class BoolConst(value: Boolean) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = builder.typesAreEqual(_type, BoolConstraintType)
}
