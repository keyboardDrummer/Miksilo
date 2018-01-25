package core.nabl.scopes.imports

import core.nabl.ConstraintSolver
import core.nabl.objects.{Declaration, DeclarationVariable, NamedDeclaration}
import core.nabl.scopes.objects.{ConcreteScope, Scope, ScopeVariable}
import core.nabl.scopes.{DeclaresScope, ScopeNode, _}

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
  }

  override def apply(solver: ConstraintSolver): Boolean = declaration match {
    case named: NamedDeclaration =>
      val edges = solver.scopeGraph.getOrElseUpdate(DeclarationNode(named), mutable.Set.empty[GraphEdge])
      val declaredEdge = edges.find(e => e.isInstanceOf[DeclaresScope])
      if (declaredEdge.nonEmpty) {
        if (!solver.unifyScopes(declaredEdge.head.asInstanceOf[DeclaresScope].target.scope, scope)) {
          return false
        }
      }
      else {
        scope match {
          case c: ConcreteScope => edges.add(DeclaresScope(ScopeNode(c)))
          case _ => return false
        }
      }
      true
    case _ => false
  }
}
