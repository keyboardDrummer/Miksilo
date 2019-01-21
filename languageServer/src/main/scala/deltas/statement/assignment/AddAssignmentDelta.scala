package deltas.statement.assignment

import core.language.node.NodeShape
import deltas.expression.additive.AdditionDelta

object AddAssignmentDelta extends OperatorWithAssignmentDelta {

  object Shape extends NodeShape

  override def operatorDelta = AdditionDelta

  override val shape = Shape
}
