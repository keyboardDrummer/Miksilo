package miksilo.languageServer.core.smarts.types.objects

import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope

trait ConstraintExpression
{
  def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit

  def getType(builder: ConstraintBuilder, parentScope: Scope): Type = {
    val result = builder.typeVariable()
    constraints(builder, result, parentScope)
    result
  }
}
