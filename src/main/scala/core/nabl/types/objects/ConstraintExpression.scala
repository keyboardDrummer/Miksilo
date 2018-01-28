package core.nabl.types.objects

import core.nabl.ConstraintBuilder
import core.nabl.scopes.objects.Scope

trait ConstraintExpression
{
  def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit

  def getType(builder: ConstraintBuilder, parentScope: Scope): Type = {
    val result = builder.typeVariable()
    constraints(builder, result, parentScope)
    result
  }
}
