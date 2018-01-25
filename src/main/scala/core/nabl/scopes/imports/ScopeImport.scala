package core.nabl.scopes.imports

import core.nabl.ConstraintSolver
import core.nabl.scopes.objects.{ConcreteScope, Scope, ScopeVariable}
import core.nabl.scopes.{ImportEdge, _}

case class ScopeImport(var importingScope: Scope, var importedScope: Scope) extends ScopeConstraint {
  override def instantiateScope(variable: ScopeVariable, instance: Scope): Unit = {
    if (importingScope == variable)
      importingScope = instance
    if (importedScope == variable)
      importedScope = instance
  }

  override def apply(solver: ConstraintSolver): Boolean = (importingScope, importedScope) match {
    case (concreteImporting: ConcreteScope, concreteImported: ConcreteScope) =>
      solver.scopeGraph.add(ScopeNode(concreteImporting), ImportEdge(ScopeNode(concreteImported))); true
    case _ => false
  }
}
