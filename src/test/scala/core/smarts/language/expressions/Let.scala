package core.smarts.language.expressions

import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import core.smarts.language.types.LanguageType

case class Let(name: String, bindingValue: Expression, value: Expression, bindingLanguageType: Option[LanguageType] = None) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit = {
    val scope = builder.newScope(Some(parentScope))
    val bindingType = bindingValue.getType(builder, scope)
    builder.declare(name, scope, this, Some(bindingType))
    value.constraints(builder, _type, scope)

    bindingLanguageType.foreach(t => {
      val bindingConstraintType = t.constraints(builder, parentScope)
      builder.typesAreEqual(bindingConstraintType, bindingType)
    })
  }
}
