package core.nabl.language.expressions

import core.nabl.constraints.ConstraintBuilder
import core.nabl.constraints.scopes.objects.Scope
import core.nabl.constraints.types.CheckSubType
import core.nabl.constraints.types.objects.{ConstraintClosureType, ConstraintExpression, Type}
import core.nabl.language.types.LanguageType

case class Lambda(name: String, body: Expression, parameterDefinedType: Option[LanguageType] = None) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit = {
    val wrappedBody = parameterDefinedType.fold[ConstraintExpression](body)(
      t => new TypeCheckWrapper(name, body, t.constraints(builder, parentScope)))
    builder.typesAreEqual(_type, ConstraintClosureType(parentScope, name, this, wrappedBody))
  }

  class TypeCheckWrapper(name: String, original: ConstraintExpression, parameterType: Type) extends ConstraintExpression
  {
    override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit = {
      val declaration = builder.resolve(name, this, parentScope)
      builder.add(CheckSubType(builder.getType(declaration), parameterType))
      original.constraints(builder, _type, parentScope)
    }
  }
}
