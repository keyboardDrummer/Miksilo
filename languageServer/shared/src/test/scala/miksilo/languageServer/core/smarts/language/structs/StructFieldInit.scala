package miksilo.languageServer.core.smarts.language.structs

import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.language.expressions.Expression
import miksilo.languageServer.core.smarts.language.modules.FakeSourcePath
import miksilo.languageServer.core.smarts.scopes.objects.Scope

case class StructFieldInit(fieldName: String, value: Expression) extends FakeSourcePath {
  def constraints(builder: ConstraintBuilder, structScope: Scope, parentScope: Scope): Unit = {
    val fieldDeclaration = builder.resolve(fieldName, structScope, this)
    val fieldType = builder.getType(fieldDeclaration)
    value.constraints(builder, fieldType, parentScope)
  }
}
