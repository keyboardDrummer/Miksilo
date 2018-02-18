package deltas.javac.methods

import core.deltas.path.NodePath
import core.smarts.objects.{Declaration, DeclarationVariable, NamedDeclaration}
import core.smarts.scopes.objects.{Scope, ScopeVariable}
import core.smarts.{Constraint, ConstraintSolver}
import deltas.javac.classes.skeleton.JavaClassSkeleton

case class SelectorTargetScopeConstraint(var targetDeclaration: Declaration, var scope: Scope) extends Constraint {
  override def apply(solver: ConstraintSolver): Boolean = {
    targetDeclaration match {
      case e:NamedDeclaration =>
        val path = e.origin.asInstanceOf[NodePath]
        val node = path.current
        val targetScope = node.shape match {
          case JavaClassSkeleton.Shape => //TODO allow referencing packages.
            solver.builder.getDeclaredScope(targetDeclaration)
          case _ =>
            val objectType = solver.builder.getType(targetDeclaration)
            val objectDeclaration = solver.builder.getDeclarationOfType(objectType)
            solver.builder.getDeclaredScope(objectDeclaration)
        }
        solver.unifyScopes(scope, targetScope) //TODO de order of the scopes matters here because the builder hasn't moved the new constraints into solver.constraints yet. Fix
        true
      case _ => false
    }
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
