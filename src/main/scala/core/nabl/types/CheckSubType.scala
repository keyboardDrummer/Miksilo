package core.nabl.types

import core.nabl.ConstraintSolver
import core.nabl.objects.{Declaration, DeclarationVariable}
import core.nabl.types.objects.{Type, TypeVariable}

case class CheckSubType(var subType: Type, var superType: Type) extends TypeConstraint {
  override def apply(solver: ConstraintSolver): Boolean = subType match {
//    case v:TypeVariable =>
//      if (!solver.boundVariables.contains(v))
//        solver.unifyTypes(subType, superType)
//      else
//        false
    case _ => solver.isSuperType(superType, subType)
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

