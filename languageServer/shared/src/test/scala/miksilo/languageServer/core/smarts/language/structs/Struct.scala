package miksilo.languageServer.core.smarts.language.structs

import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.language.modules.FakeSourcePath
import miksilo.languageServer.core.smarts.objects.NamedDeclaration
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.{AssignSubType, DeclarationHasType}
import miksilo.languageServer.core.smarts.types.objects.TypeFromDeclaration

trait TypeDefinition
{
  def constraints(builder: ConstraintBuilder, parentScope: Scope) : Unit
}

case class Struct(name: String, fields: Seq[Field], maybeParent: Option[String] = None, typeParameter: Option[String] = None)
  extends TypeDefinition with FakeSourcePath
{
  def constraints(builder: ConstraintBuilder, parentScope: Scope): Unit =
  {
    val structDeclaration: NamedDeclaration = builder.declare(name, parentScope, this)
    builder.add(DeclarationHasType(structDeclaration, TypeFromDeclaration(structDeclaration)))
    val scopeOfParent: Option[Scope] = maybeParent.map(parent => {
      val parentDeclaration = builder.resolve(parent, parentScope, this)
      val scopeOfParent = builder.getDeclaredScope(parentDeclaration)

      builder.add(List(AssignSubType(TypeFromDeclaration(structDeclaration), TypeFromDeclaration(parentDeclaration))))
      scopeOfParent
    })
    val structScope = builder.declareScope(structDeclaration, scopeOfParent.orNull)
    fields.foreach(field => {
      val _type = field._type.constraints(builder, parentScope)
      builder.declare(field.name, structScope, field, Some(_type))
    })
  }

  override def toString = s"Struct($name)"
}
