package core.smarts.scopes

import core.smarts.{ConstraintSolver, SolveException}
import core.smarts.objects.NamedDeclaration
import core.smarts.scopes.objects.{ConcreteScope, Scope, ScopeVariable}

case class DuplicateDeclarationException(declaration: NamedDeclaration) extends SolveException
case class DeclarationInsideScope(var declaration: NamedDeclaration, var scope: Scope) extends ScopeConstraint {
  override def instantiateScope(variable: ScopeVariable, instance: Scope): Unit = {
    if (scope == variable)
      scope = instance
  }

  override def apply(solver: ConstraintSolver): Boolean = scope match {
    case concrete: ConcreteScope =>
      val duplicate = solver.scopeGraph.add(concrete, DeclaresDeclaration(declaration))
      if (duplicate && !solver.allowDuplicateDeclaration)
        throw DuplicateDeclarationException(declaration)
      true
    case _ => false
  }
}
