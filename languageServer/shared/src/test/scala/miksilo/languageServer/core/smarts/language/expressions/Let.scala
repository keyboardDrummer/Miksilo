package miksilo.languageServer.core.smarts.language.expressions

import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.Type
import miksilo.languageServer.core.smarts.language.types.LanguageType

case class Let(name: String, bindingValue: Expression, value: Expression, bindingLanguageType: Option[LanguageType] = None) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit = {
    val scope = builder.newScope(parentScope)
    val bindingType = bindingValue.getType(builder, scope)
    builder.declare(name, scope, this, Some(bindingType))
    value.constraints(builder, _type, scope)

    bindingLanguageType.foreach(t => {
      val bindingConstraintType = t.constraints(builder, parentScope)
      builder.typesAreEqual(bindingConstraintType, bindingType)
    })
  }
}
