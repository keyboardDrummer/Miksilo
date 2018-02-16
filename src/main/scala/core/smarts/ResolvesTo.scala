package core.smarts

import core.smarts.objects.{Declaration, DeclarationVariable, Reference}
import core.smarts.scopes.ResolutionConstraint
case class ResolvesTo(reference: Reference, var declaration: Declaration) extends ResolutionConstraint
{
  override def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration): Unit = {
    if (declaration == variable)
      declaration = instance
  }

  override def apply(solver: ConstraintSolver): Boolean = {
    val resolvedDeclaration = solver.scopeGraph.resolve(reference)
    if (resolvedDeclaration == null)
      return false

    if (!solver.unifyDeclarations(declaration, resolvedDeclaration))
    {
      throw new IllegalStateException("what?!")
    }
    true
  }
}
