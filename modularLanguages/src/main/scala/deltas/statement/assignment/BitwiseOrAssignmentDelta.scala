package deltas.statement.assignment

import core.language.node.NodeShape
import deltas.expression.bitwise.BitwiseOrDelta

object BitwiseOrAssignmentDelta extends OperatorWithAssignmentDelta {

  override def operatorDelta = BitwiseOrDelta

  object Shape extends NodeShape
  override val shape = Shape
}
