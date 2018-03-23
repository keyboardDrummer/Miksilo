package core.smarts.language.modules

import core.smarts.ConstraintBuilder
import core.smarts.language.expressions.Expression
import core.smarts.language.types.LanguageType
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type

case class Binding(name: String, body: Expression, bindingType: Option[LanguageType] = None) extends FakeSourceElement
{
  def constraints(builder: ConstraintBuilder, parentScope: Scope): Unit = {
    val typeVariable = bindingType.fold[Type](builder.typeVariable())(t => t.constraints(builder, parentScope))
    builder.declare(name, parentScope, this, Some(typeVariable))
    body.constraints(builder, typeVariable, parentScope)
  }
}
