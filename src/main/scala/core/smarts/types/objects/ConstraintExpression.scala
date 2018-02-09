package core.smarts.types.objects

import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope

trait ConstraintExpression
{
  def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit

  def getType(builder: ConstraintBuilder, parentScope: Scope): Type = {
    val result = builder.typeVariable()
    constraints(builder, result, parentScope)
    result
  }
}
