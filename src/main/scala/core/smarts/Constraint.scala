package core.smarts

import core.smarts.objects.{Declaration, DeclarationVariable}
import core.smarts.scopes.objects.{Scope, ScopeVariable}
import core.smarts.types.objects.{Type, TypeVariable}

trait Constraint {
  def apply(solver: ConstraintSolver): Boolean

  def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration) = {}
  def instantiateType(variable: TypeVariable, instance: Type) = {}
  def instantiateScope(variable: ScopeVariable, instance: Scope) = {}
  def boundTypes: Set[Type] = Set.empty
}
