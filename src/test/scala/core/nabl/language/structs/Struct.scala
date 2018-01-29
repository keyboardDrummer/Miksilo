package core.nabl.language.structs

import core.nabl.ConstraintBuilder
import core.nabl.language.modules.FakeSourceElement
import core.nabl.objects.NamedDeclaration
import core.nabl.scopes.objects.Scope
import core.nabl.types.{AssignSubType, DeclarationOfType}
import core.nabl.types.objects.TypeFromDeclaration

trait TypeDefinition
{
  def constraints(builder: ConstraintBuilder, parentScope: Scope) : Unit
}

case class Struct(name: String, fields: Seq[Field], parent: Option[String] = None, typeParameter: Option[String] = None)
  extends TypeDefinition with FakeSourceElement
{
  def constraints(builder: ConstraintBuilder, parentScope: Scope): Unit =
  {
    val structDeclaration: NamedDeclaration = builder.declaration(name, this, parentScope)
    builder.add(DeclarationOfType(structDeclaration, TypeFromDeclaration(structDeclaration)))
    val scopeOfParent: Option[Scope] = parent.map(p => {
      val parentDeclaration = builder.declarationVariable()
      val scopeOfParent = builder.declaredScopeVariable(parentDeclaration)
      builder.reference(p, this, parentScope, parentDeclaration)
      builder.add(List(AssignSubType(TypeFromDeclaration(structDeclaration), TypeFromDeclaration(parentDeclaration))))
      scopeOfParent
    })
    val structScope = builder.declaredNewScope(structDeclaration, scopeOfParent)
    fields.foreach(field => {
      val _type = field._type.constraints(builder, parentScope)
      builder.declaration(field.name, field, structScope, Some(_type))
    })
  }

  override def toString = s"Struct($name)"
}
