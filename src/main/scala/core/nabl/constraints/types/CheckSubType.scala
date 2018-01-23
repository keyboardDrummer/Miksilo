package core.nabl.constraints.types

import core.nabl.constraints.ConstraintSolver
import core.nabl.constraints.objects.{Declaration, DeclarationVariable}
import core.nabl.constraints.types.objects.{ConcreteType, Type, TypeVariable}

case class CheckSubType(var subType: Type, var superType: Type) extends TypeConstraint {
  override def apply(solver: ConstraintSolver): Boolean = subType match {
//    case v:TypeVariable =>
//      if (!solver.boundVariables.contains(v))
//        solver.unifyTypes(subType, superType)
//      else
//        false
    case _ => solver.canAssignTo(superType, subType)
  }

  override def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration): Unit = {
    subType.instantiateDeclaration(variable, instance)
    superType.instantiateDeclaration(variable, instance)
  }

  override def instantiateType(variable: TypeVariable, instance: Type): Unit = {
    subType = subType.instantiateType(variable, instance)
    superType = superType.instantiateType(variable, instance)
  }

  override def boundTypes: Set[Type] = Set(subType, superType)
}

case class AssignSubType(var subType: Type, var superType: Type) extends TypeConstraint {
  override def apply(solver: ConstraintSolver): Boolean = {
    if (subType.fullyApplied && superType.fullyApplied)
    {
      solver.typeGraph.add(TypeNode(subType), SuperType(TypeNode(superType)))
      true
    }
    else false
  }

  override def instantiateType(variable: TypeVariable, instance: Type): Unit = {
    subType = subType.instantiateType(variable, instance)
    superType = superType.instantiateType(variable, instance)
  }

  override def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration): Unit = {
    subType.instantiateDeclaration(variable, instance)
    superType.instantiateDeclaration(variable, instance)
  }

  override def boundTypes: Set[Type] = Set(subType, superType)
}