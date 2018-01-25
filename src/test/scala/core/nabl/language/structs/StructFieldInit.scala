package core.nabl.language.structs

import core.nabl.ConstraintBuilder
import core.nabl.language.expressions.Expression
import core.nabl.scopes.objects.Scope

case class StructFieldInit(fieldName: String, value: Expression) {
  def constraints(builder: ConstraintBuilder, structScope: Scope, parentScope: Scope): Unit = {
    val fieldDeclaration = builder.resolve(fieldName, this, structScope)
    val fieldType = builder.getType(fieldDeclaration)
    value.constraints(builder, fieldType, parentScope)
  }
}
