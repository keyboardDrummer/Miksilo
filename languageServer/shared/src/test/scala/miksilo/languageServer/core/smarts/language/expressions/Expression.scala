package miksilo.languageServer.core.smarts.language.expressions

import miksilo.languageServer.core.smarts.language.modules.FakeSourcePath
import miksilo.languageServer.core.smarts.language.structs.Access
import miksilo.languageServer.core.smarts.types.objects.ConstraintExpression

trait Expression extends ConstraintExpression with FakeSourcePath {

  def apply(argument: Expression) = Application(this, argument)

  def access(field: String) = Access(this, field)
}
