package core.smarts.language.structs

import core.smarts.ConstraintBuilder
import core.smarts.language.expressions.Expression
import core.smarts.language.modules.FakeSourceElement
import core.smarts.scopes.objects.Scope

case class StructFieldInit(fieldName: String, value: Expression) extends FakeSourceElement {
  def constraints(builder: ConstraintBuilder, structScope: Scope, parentScope: Scope): Unit = {
    val fieldDeclaration = builder.resolve(fieldName, this, structScope)
    val fieldType = builder.getType(fieldDeclaration)
    value.constraints(builder, fieldType, parentScope)
  }
}
