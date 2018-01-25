package core.nabl.types

import core.nabl.ConstraintSolver
import core.nabl.objects.{Declaration, DeclarationVariable, NamedDeclaration}
import core.nabl.types.objects.{Type, TypeVariable}

case class DeclarationOfType(var declaration: Declaration, var _type: Type) extends TypeConstraint {
  override def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration): Unit = {
    _type.instantiateDeclaration(variable, instance)
    if (declaration == variable)
      declaration = instance
  }

  override def instantiateType(variable: TypeVariable, instance: Type): Unit = {
    _type = _type.instantiateType(variable, instance)
  }

  override def boundTypes: Set[Type] = Set(_type)

  override def apply(solver: ConstraintSolver): Boolean = declaration match {
    case named: NamedDeclaration => solver.declare(named, _type)
    case _ => false
  }
}
