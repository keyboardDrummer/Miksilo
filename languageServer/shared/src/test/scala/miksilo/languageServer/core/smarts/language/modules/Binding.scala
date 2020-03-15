package miksilo.languageServer.core.smarts.language.modules

import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.language.expressions.Expression
import miksilo.languageServer.core.smarts.language.types.LanguageType
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.Type

case class Binding(name: String, body: Expression, bindingType: Option[LanguageType] = None) extends FakeSourcePath
{
  def constraints(builder: ConstraintBuilder, parentScope: Scope): Unit = {
    val typeVariable = bindingType.fold[Type](builder.typeVariable())(t => t.constraints(builder, parentScope))
    builder.declare(name, parentScope, this, Some(typeVariable))
    body.constraints(builder, typeVariable, parentScope)
  }
}
