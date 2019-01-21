package deltas.statement.assignment

import core.language.node.NodeShape
import deltas.expression.multiplicative.MultiplyDelta

object MultiplyAssignmentDelta extends OperatorWithAssignmentDelta {

  override def operatorDelta = MultiplyDelta

  object Shape extends NodeShape
  override val shape = Shape
}
