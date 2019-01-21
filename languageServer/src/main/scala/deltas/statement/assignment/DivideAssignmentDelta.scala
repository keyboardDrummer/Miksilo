package deltas.statement.assignment

import core.language.node.NodeShape
import deltas.expression.multiplicative.DivideDelta

object DivideAssignmentDelta extends OperatorWithAssignmentDelta {

  override def operatorDelta = DivideDelta

  object Shape extends NodeShape
  override val shape = Shape
}
