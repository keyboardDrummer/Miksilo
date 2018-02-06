package core.nabl

import core.nabl.objects.{Declaration, DeclarationVariable, Reference}
import core.nabl.scopes.ResolutionConstraint

case class ResolvesTo(reference: Reference, var declaration: Declaration) extends ResolutionConstraint
{
  override def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration): Unit = {
    if (declaration == variable)
      declaration = instance
  }

  override def apply(solver: ConstraintSolver): Boolean = {
    if (reference.name == "java.lang")
      System.out.print("")
    val resolvedDeclaration = solver.scopeGraph.resolve(reference)
    if (resolvedDeclaration != null)
    {
      if (!solver.unifyDeclarations(declaration, resolvedDeclaration))
      {
        throw new IllegalStateException("what?!")
      }
      true
    }
    else
    {
      false
    }
  }
}
