package core.smarts.language.expressions

import core.smarts.language.modules.FakeSourceElement
import core.smarts.language.structs.Access
import core.smarts.types.objects.ConstraintExpression

trait Expression extends ConstraintExpression with FakeSourceElement {

  def apply(argument: Expression) = Application(this, argument)

  def access(field: String) = Access(this, field)
}
