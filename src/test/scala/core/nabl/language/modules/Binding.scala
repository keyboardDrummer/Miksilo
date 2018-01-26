package core.nabl.language.modules

import core.nabl.ConstraintBuilder
import core.nabl.language.expressions.Expression
import core.nabl.language.types.LanguageType
import core.nabl.scopes.objects.Scope
import core.nabl.types.objects.Type

case class Binding(name: String, body: Expression, bindingType: Option[LanguageType] = None) extends FakeSourceElement
{
  def constraints(builder: ConstraintBuilder, parentScope: Scope): Unit = {
    val typeVariable = bindingType.fold[Type](builder.typeVariable())(t => t.constraints(builder, parentScope))
    builder.declaration(name, this, parentScope, Some(typeVariable))
    body.constraints(builder, typeVariable, parentScope)
  }
}
