package core.nabl.language.structs

import core.nabl.ConstraintBuilder
import core.nabl.scopes.objects.Scope
import core.nabl.types.CheckSubType
import core.nabl.types.objects.{TypeFromDeclaration, Type}
import core.nabl.language.expressions.Expression

case class TypeCase(typeName: String, bindingName: String, body: Expression)
case class MatchType(subject: Expression, cases: Seq[TypeCase]) extends Expression {

  override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit = {
    val subjectType = subject.getType(builder, parentScope)
    cases.foreach(_case => {
      val caseTypeDeclaration = builder.resolve(_case.typeName, this, parentScope)
      val caseType = builder.getType(caseTypeDeclaration)
      builder.add(CheckSubType(caseType, subjectType))
      val bodyScope = builder.newScope(Some(parentScope))
      builder.declaration(_case.bindingName, this, bodyScope, Some(caseType))
      builder.typesAreEqual(_type, _case.body.getType(builder, bodyScope))
    })
  }
}

case class Access(target: Expression, field: String) extends Expression
{
  /* We don't need a scope import because we can directly use the struct scope to resolve the member.
   */
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    val structDeclaration = builder.declarationVariable()
    val fieldDeclaration = builder.declarationVariable(_type)
    val structScope = builder.declaredScopeVariable(structDeclaration)
    builder.reference(field, this, structScope, fieldDeclaration)
    target.constraints(builder, TypeFromDeclaration(structDeclaration), scope)
  }
}
