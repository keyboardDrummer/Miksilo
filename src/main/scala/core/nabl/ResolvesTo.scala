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
    val resolvedDeclaration = solver.scopeGraph.resolve(reference)
    if (resolvedDeclaration != null)
    {
      if (!solver.unifyDeclarations(declaration, resolvedDeclaration))
      {
        throw new IllegalStateException("what?!")
      }
      true
    }
    else if (reference.name == "Class") //. De java.util import faalt nog. Maar ook de class references lijken niet te resolven.
    {
      val resolvedDeclaration2 = solver.scopeGraph.resolve(reference)
      false
    }
    else
    {
      false
    }
  }
}
