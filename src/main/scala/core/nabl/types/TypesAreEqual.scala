package core.nabl.types

import core.nabl.ConstraintSolver
import core.nabl.objects.{Declaration, DeclarationVariable}
import core.nabl.types.objects.{Type, TypeVariable}

case class TypesAreEqual(var left: Type, var right: Type) extends TypeConstraint {

  override def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration): Unit = {
    left.instantiateDeclaration(variable, instance)
    right.instantiateDeclaration(variable, instance)
  }

  override def instantiateType(variable: TypeVariable, instance: Type): Unit = {
    left = left.instantiateType(variable, instance)
    right = right.instantiateType(variable, instance)
  }

  override def boundTypes: Set[Type] = Set(left, right)

  override def apply(solver: ConstraintSolver): Boolean = solver.unifyTypes(left,  right)
}
