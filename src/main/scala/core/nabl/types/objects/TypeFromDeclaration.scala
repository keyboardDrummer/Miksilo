package core.nabl.types.objects

import core.nabl.objects.{Declaration, DeclarationVariable, NamedDeclaration}

case class TypeFromDeclaration(var declaration: Declaration) extends ConcreteType
{
  override def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration): Unit = {
    if (declaration == variable)
      declaration = instance
  }

  override def variables: Set[TypeVariable] = Set.empty

  override def specialize(mapping: Map[TypeVariable, TypeVariable]): Type = this

  override def instantiateType(variable: TypeVariable, instance: Type): Type = this

  override def fullyApplied: Boolean = declaration.isInstanceOf[NamedDeclaration]
}
