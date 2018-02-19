package deltas.javac.methods

import core.language.SourceElement
import core.language.node.Node
import core.smarts.objects.{Declaration, DeclarationVariable, NamedDeclaration}
import core.smarts.scopes.objects.{Scope, ScopeVariable}
import core.smarts.{Constraint, ConstraintSolver}
import deltas.javac.classes.skeleton.JavaClassSkeleton

case class SelectorTargetScopeConstraint(var targetDeclaration: Declaration, var scope: Scope) extends Constraint {
  override def apply(solver: ConstraintSolver): Boolean = {
    targetDeclaration match {
      case e:NamedDeclaration =>
        val path = e.origin.asInstanceOf[SourceElement]
        val value = path.current
        val targetScope = value match {
          case node: Node => node.shape match {
            case JavaClassSkeleton.Shape => //TODO allow referencing packages.
              solver.builder.getDeclaredScope(targetDeclaration)
            case _ =>
              getScopeForExpression(solver)
          }
          case _ => getScopeForExpression(solver)
        }
        solver.unifyScopes(scope, targetScope) //TODO de order of the scopes matters here because the builder hasn't moved the new constraints into solver.constraints yet. Fix
        true
      case _ => false
    }
  }

  private def getScopeForExpression(solver: ConstraintSolver) = {
    val objectType = solver.builder.getType(targetDeclaration)
    val objectDeclaration = solver.builder.getDeclarationOfType(objectType)
    solver.builder.getDeclaredScope(objectDeclaration)
  }

  override def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration): Unit = {
    if (targetDeclaration == variable)
      targetDeclaration = instance
    super.instantiateDeclaration(variable, instance)
  }

  override def instantiateScope(variable: ScopeVariable, instance: Scope): Unit = {
    if (scope == variable)
      scope = instance
    super.instantiateScope(variable, instance)
  }
}
