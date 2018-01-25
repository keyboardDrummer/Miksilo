package core.nabl.scopes

import core.nabl.ConstraintSolver
import core.nabl.scopes.objects.{ConcreteScope, Scope, ScopeVariable}
import core.nabl.objects.NamedDeclaration

case class DeclarationInsideScope(var declaration: NamedDeclaration, var scope: Scope) extends ScopeConstraint {
  override def instantiateScope(variable: ScopeVariable, instance: Scope): Unit = {
    if (scope == variable)
      scope = instance
  }

  override def apply(solver: ConstraintSolver): Boolean = scope match {
    case concrete: ConcreteScope => solver.scopeGraph.add(ScopeNode(concrete), DeclaresDeclaration(DeclarationNode(declaration))); true
    case _ => false
  }
}
