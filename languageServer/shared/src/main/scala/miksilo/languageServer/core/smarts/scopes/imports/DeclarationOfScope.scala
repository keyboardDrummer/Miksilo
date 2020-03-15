package miksilo.languageServer.core.smarts.scopes.imports

import miksilo.languageServer.core.smarts.ConstraintSolver
import miksilo.languageServer.core.smarts.objects.{Declaration, DeclarationVariable, NamedDeclaration}
import miksilo.languageServer.core.smarts.scopes.{DeclaresScope, GraphEdge, ResolutionConstraint}
import miksilo.languageServer.core.smarts.scopes.objects.{ConcreteScope, Scope, ScopeVariable}

import scala.collection.mutable

case class DeclarationOfScope(var declaration: Declaration, var scope: Scope) extends ResolutionConstraint
{
  override def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration): Unit = {
    if (declaration == variable)
      declaration = instance
  }

  override def instantiateScope(variable: ScopeVariable, instance: Scope): Unit = {
    if (scope == variable)
      scope = instance
    super.instantiateScope(variable, instance)
  }

  override def apply(solver: ConstraintSolver): Boolean = declaration match {
    case named: NamedDeclaration =>
      val edges = solver.scopeGraph.nodes.getOrElseUpdate(named, mutable.Set.empty[GraphEdge])
      val declaredEdge = edges.find(e => e.isInstanceOf[DeclaresScope])
      if (declaredEdge.nonEmpty) {
        if (!solver.unifyScopes(declaredEdge.head.asInstanceOf[DeclaresScope].target, scope)) {
          return false
        }
      }
      else {
        scope match {
          case c: ConcreteScope => edges.add(DeclaresScope(c))
          case _ => return false
        }
      }
      true
    case _ => false
  }
}
