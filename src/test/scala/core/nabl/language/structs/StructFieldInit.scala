package core.nabl.language.structs

import core.nabl.constraints.objects.Reference
import core.nabl.constraints.scopes.ReferenceInScope
import core.nabl.constraints.scopes.objects.Scope
import core.nabl.constraints.types.DeclarationOfType
import core.nabl.constraints.{Constraint, ConstraintBuilder, ResolvesTo}
import core.nabl.language.expressions.Expression

case class StructFieldInit(fieldName: String, value: Expression) {
  def constraints(builder: ConstraintBuilder, structScope: Scope, parentScope: Scope): Unit = {
    val fieldDeclaration = builder.resolve(fieldName, this, structScope)
    val fieldType = builder.getType(fieldDeclaration)
    value.constraints(builder, fieldType, parentScope)
  }
}
