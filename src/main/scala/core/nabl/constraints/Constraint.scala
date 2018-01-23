package core.nabl.constraints

import core.nabl.constraints.objects._
import core.nabl.constraints.scopes.objects._
import core.nabl.constraints.types.objects._

trait Constraint {
  def apply(solver: ConstraintSolver): Boolean

  def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration) = {}
  def instantiateType(variable: TypeVariable, instance: Type) = {}
  def instantiateScope(variable: ScopeVariable, instance: Scope) = {}
  def boundTypes: Set[Type] = Set.empty
}
