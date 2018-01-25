package core.nabl.language.expressions

import core.nabl.ConstraintBuilder
import core.nabl.scopes.objects.Scope
import core.nabl.types.objects.Type
import core.nabl.language.types.LanguageType

case class Let(name: String, bindingValue: Expression, value: Expression, bindingLanguageType: Option[LanguageType] = None) extends Expression {
  override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit = {
    val scope = builder.newScope(Some(parentScope))
    val bindingType = bindingValue.getType(builder, scope)
    builder.declaration(name, this, scope, Some(bindingType))
    value.constraints(builder, _type, scope)

    bindingLanguageType.foreach(t => {
      val bindingConstraintType = t.constraints(builder, parentScope)
      builder.typesAreEqual(bindingConstraintType, bindingType)
    })
  }
}
