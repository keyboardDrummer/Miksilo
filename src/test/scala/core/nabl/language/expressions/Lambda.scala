package core.nabl.language.expressions

import core.language.SourceElement
import core.nabl.ConstraintBuilder
import core.nabl.language.types.LanguageType
import core.nabl.scopes.objects.Scope
import core.nabl.types.CheckSubType
import core.nabl.types.objects.{ConstraintClosureType, ConstraintExpression, Type}

case class Lambda(name: String, body: Expression, parameterDefinedType: Option[LanguageType] = None) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit = {
    val wrappedBody = parameterDefinedType.fold[ConstraintExpression](body)(
      t => new TypeCheckWrapper(name, this /* TODO should be the argument's type definition */, body, t.constraints(builder, parentScope)))
    builder.typesAreEqual(_type, ConstraintClosureType(parentScope, name, this, wrappedBody))
  }

  class TypeCheckWrapper(name: String, location: SourceElement,original: ConstraintExpression, parameterType: Type) extends ConstraintExpression with SourceElement
  {
    override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit = {
      val declaration = builder.resolve(name, location, parentScope)
      builder.add(CheckSubType(builder.getType(declaration), parameterType))
      original.constraints(builder, _type, parentScope)
    }
  }
}
