package miksilo.languageServer.core.smarts.language.expressions

import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.language.modules.FakeSourcePath
import miksilo.languageServer.core.smarts.language.types.LanguageType
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.CheckSubType
import miksilo.languageServer.core.smarts.types.objects.{ConstraintClosureType, ConstraintExpression, Type}
import miksilo.languageServer.server.SourcePath

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
