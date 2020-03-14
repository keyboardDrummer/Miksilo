package core.smarts.language.expressions

import core.smarts.ConstraintBuilder
import core.smarts.language.modules.FakeSourcePath
import core.smarts.language.types.LanguageType
import core.smarts.scopes.objects.Scope
import core.smarts.types.CheckSubType
import core.smarts.types.objects.{ConstraintClosureType, ConstraintExpression, Type}
import languageServer.SourcePath

case class Lambda(name: String, body: Expression, parameterDefinedType: Option[LanguageType] = None) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit = {
    val wrappedBody = parameterDefinedType.fold[ConstraintExpression](body)(
      t => new TypeCheckWrapper(name, this /* TODO should be the argument's type definition */, body, t.constraints(builder, parentScope)))
    builder.typesAreEqual(_type, ConstraintClosureType(parentScope, name, this, wrappedBody))
  }

  class TypeCheckWrapper(name: String, location: SourcePath, original: ConstraintExpression, parameterType: Type)
    extends ConstraintExpression with FakeSourcePath
  {
    override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit = {
      val declaration = builder.resolve(name, parentScope, location)
      builder.add(CheckSubType(builder.getType(declaration), parameterType))
      original.constraints(builder, _type, parentScope)
    }
  }
}
