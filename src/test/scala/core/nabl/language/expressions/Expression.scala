package core.nabl.language.expressions

import core.nabl.constraints.types.objects.ConstraintExpression
import core.nabl.language.structs.Access

trait Expression extends ConstraintExpression {

  def apply(argument: Expression) = Application(this, argument)

  def access(field: String) = Access(this, field)

}
