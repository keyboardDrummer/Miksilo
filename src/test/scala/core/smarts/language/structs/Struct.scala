package core.smarts.language.structs

import core.smarts.ConstraintBuilder
import core.smarts.language.modules.FakeSourceElement
import core.smarts.objects.NamedDeclaration
import core.smarts.scopes.objects.Scope
import core.smarts.types.{AssignSubType, DeclarationOfType}
import core.smarts.types.objects.TypeFromDeclaration

trait TypeDefinition
{
  def constraints(builder: ConstraintBuilder, parentScope: Scope) : Unit
}

case class Struct(name: String, fields: Seq[Field], parent: Option[String] = None, typeParameter: Option[String] = None)
  extends TypeDefinition with FakeSourceElement
{
  def constraints(builder: ConstraintBuilder, parentScope: Scope): Unit =
  {
    val structDeclaration: NamedDeclaration = builder.declare(name, this, parentScope)
    builder.add(DeclarationOfType(structDeclaration, TypeFromDeclaration(structDeclaration)))
    val scopeOfParent: Option[Scope] = parent.map(p => {
      val parentDeclaration = builder.declarationVariable()
      val scopeOfParent = builder.resolveScopeDeclaration(parentDeclaration)
      builder.reference(p, this, parentScope, parentDeclaration)
      builder.add(List(AssignSubType(TypeFromDeclaration(structDeclaration), TypeFromDeclaration(parentDeclaration))))
      scopeOfParent
    })
    val structScope = builder.declareScope(structDeclaration, scopeOfParent)
    fields.foreach(field => {
      val _type = field._type.constraints(builder, parentScope)
      builder.declare(field.name, field, structScope, Some(_type))
    })
  }

  override def toString = s"Struct($name)"
}
