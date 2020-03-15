package miksilo.languageServer.core.smarts

import miksilo.languageServer.core.smarts.objects.{Declaration, Reference}
import miksilo.languageServer.core.smarts.types.objects.{Type, TypeVariable}

class ResolvesToType(reference: Reference, declaration: Declaration, var _type: Type)
  extends ResolvesTo(reference, declaration)
{
  override def apply(solver: ConstraintSolver): Boolean = {
    val initialDeclarations = solver.scopeGraph.resolve(reference)

    val declarations = initialDeclarations.filter(declaration => {
      solver.environment.get(declaration).fold(false)(declarationType => {
        solver.couldBeSuperType(_type, declarationType)
      })
    })
    val result = applyDeclarations(solver, declarations)
    if (result ) {
      val declarationType = solver.environment(declarations.head)
      solver.unifyTypes(_type, declarationType) //TODO moet assignSuperType zijn.
    }
    result
  }

  override def instantiateType(variable: TypeVariable, instance: Type): Unit = {
    _type = _type.instantiateType(variable, instance)
    super.instantiateType(variable, instance)
  }

  override def boundTypes: Set[Type] = Set(_type)
}
