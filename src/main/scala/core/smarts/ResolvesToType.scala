package core.smarts

import core.smarts.objects.{Declaration, Reference}
import core.smarts.types.objects.{Type, TypeVariable}

class ResolvesToType(reference: Reference, declaration: Declaration, var _type: Type) extends ResolvesTo(reference, declaration)
{
  override def apply(solver: ConstraintSolver): Boolean = {
    val initialDeclarations = solver.scopeGraph.resolve(reference)

    if (initialDeclarations.length > 0 && reference.name == "print")
      System.out.append("jo")

    val declarations = initialDeclarations.filter(declaration => {
      solver.environment.get(declaration).fold(faalse)(declarationType => {
        solver.couldBeSuperType(_type, declarationType)
      })
    })
    applyDeclarations(solver, declarations)
  }

  override def instantiateType(variable: TypeVariable, instance: Type): Unit = {
    _type = _type.instantiateType(variable, instance)
    super.instantiateType(variable, instance)
  }

}
