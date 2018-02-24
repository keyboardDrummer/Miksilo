package core.smarts

import core.smarts.objects.{Declaration, DeclarationVariable, NamedDeclaration, Reference}
import core.smarts.scopes.ResolutionConstraint

case class ResolvesTo(reference: Reference, var declaration: Declaration) extends ResolutionConstraint
{
  override def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration): Unit = {
    if (declaration == variable)
      declaration = instance
  }

  override def apply(solver: ConstraintSolver): Boolean = {
    val declarations = solver.scopeGraph.resolve(reference)
    applyDeclarations(solver, declarations)
  }

  def applyDeclarations(solver: ConstraintSolver, declarations: Seq[NamedDeclaration]): Boolean = {
    if (declarations.length == 1) {
      val resolvedDeclaration = declarations.head
      if (!solver.unifyDeclarations(declaration, resolvedDeclaration)) //TODO maybe we don't need ResolvesToType. If we can store the type of a variable declaration, then we can replace ResolvesToType with ResolvesTo and DeclarationHasType.
        throw new IllegalStateException("what?!")

      solver.proofs.resolutions += reference -> resolvedDeclaration
      true
    }
    else if (declarations.length > 1)
      false
    else
      false
  }
}

