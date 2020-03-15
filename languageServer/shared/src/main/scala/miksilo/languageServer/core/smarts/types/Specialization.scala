package miksilo.languageServer.core.smarts.types

import miksilo.languageServer.core.smarts.ConstraintSolver
import miksilo.languageServer.core.smarts.types.objects.{ConcreteType, Poly, Type, TypeVariable}

case class Specialization(var specialized: Type, var template: Type, debugInfo: Any = null) extends TypeConstraint
{
  override def instantiateType(variable: TypeVariable, instance: Type): Unit = {
    specialized = specialized.instantiateType(variable, instance)
    template = template.instantiateType(variable, instance)
  }

  override def boundTypes: Set[Type] = Set(specialized)

  override def apply(solver: ConstraintSolver): Boolean = {
    template match {
      case poly: Poly =>
        val instantiatedTemplate = poly.specialize(poly.arguments.map(v => (v, solver.builder.typeVariable())).toMap)
        solver.unifyTypes(specialized, instantiatedTemplate)
      case _: ConcreteType =>
        solver.unifyTypes(specialized, template)
      case _ =>
        if (solver.boundVariables.intersect(template.variables).isEmpty)
          solver.unifyTypes(specialized, template)
        else
          false
    }
  }
}
