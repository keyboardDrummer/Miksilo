package miksilo.languageServer.core.smarts.scopes.imports

import miksilo.languageServer.core.smarts.ConstraintSolver
import miksilo.languageServer.core.smarts.scopes.objects.{ConcreteScope, Scope, ScopeVariable}
import miksilo.languageServer.core.smarts.scopes.{ImportEdge, _}

case class ScopeImport(var importingScope: Scope, var importedScope: Scope) extends ScopeConstraint {
  override def instantiateScope(variable: ScopeVariable, instance: Scope): Unit = {
    if (importingScope == variable)
      importingScope = instance
    if (importedScope == variable)
      importedScope = instance
  }

  override def apply(solver: ConstraintSolver): Boolean = (importingScope, importedScope) match {
    case (concreteImporting: ConcreteScope, concreteImported: ConcreteScope) =>
      solver.scopeGraph.addEdge(concreteImporting, ImportEdge(concreteImported)); true
    case _ => false
  }
}
