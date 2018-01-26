package core.nabl.language.expressions

import core.nabl.language.modules.FakeSourceElement
import core.nabl.language.structs.Access
import core.nabl.types.objects.ConstraintExpression

trait Expression extends ConstraintExpression with FakeSourceElement {

  def apply(argument: Expression) = Application(this, argument)

  def access(field: String) = Access(this, field)
}
