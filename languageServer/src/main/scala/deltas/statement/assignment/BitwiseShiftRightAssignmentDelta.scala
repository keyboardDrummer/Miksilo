package deltas.statement.assignment

import core.language.node.NodeShape
import deltas.expression.bitwise.BitwiseShiftRightDelta

object BitwiseShiftRightAssignmentDelta extends OperatorWithAssignmentDelta {

  override def operatorDelta = BitwiseShiftRightDelta

  object Shape extends NodeShape
  override val shape = Shape
}
