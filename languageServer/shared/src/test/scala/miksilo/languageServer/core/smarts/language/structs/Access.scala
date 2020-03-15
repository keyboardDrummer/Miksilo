package miksilo.languageServer.core.smarts.language.structs

import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.CheckSubType
import miksilo.languageServer.core.smarts.types.objects.{TypeFromDeclaration, Type}
import miksilo.languageServer.core.smarts.language.expressions.Expression

case class TypeCase(typeName: String, bindingName: String, body: Expression)
case class MatchType(subject: Expression, cases: Seq[TypeCase]) extends Expression {

  override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit = {
    val subjectType = subject.getType(builder, parentScope)
    cases.foreach(_case => {
      val caseTypeDeclaration = builder.resolve(_case.typeName, parentScope, this)
      val caseType = builder.getType(caseTypeDeclaration)
      builder.add(CheckSubType(caseType, subjectType))
      val bodyScope = builder.newScope(parentScope)
      builder.declare(_case.bindingName, bodyScope, this, Some(caseType))
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
    val structScope = builder.getDeclaredScope(structDeclaration)
    builder.resolve(field, structScope, this, Some(_type))
    target.constraints(builder, TypeFromDeclaration(structDeclaration), scope)
  }
}
