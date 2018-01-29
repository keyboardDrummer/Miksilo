package core.nabl.scopes

import core.nabl.ConstraintSolver
import core.nabl.objects.NamedDeclaration
import core.nabl.scopes.objects.{ConcreteScope, Scope, ScopeVariable}

case class DeclarationInsideScope(var declaration: NamedDeclaration, var scope: Scope) extends ScopeConstraint {
  override def instantiateScope(variable: ScopeVariable, instance: Scope): Unit = {
    if (scope == variable)
      scope = instance
  }

  override def apply(solver: ConstraintSolver): Boolean = scope match {
    case concrete: ConcreteScope =>
      val duplicate = solver.scopeGraph.add(ScopeNode(concrete), DeclaresDeclaration(DeclarationNode(declaration)))
//      if (duplicate && !solver.allowDuplicateDeclaration)
//        throw SolveException
      true
    case _ => false
  }
}
