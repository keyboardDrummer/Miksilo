package deltas.statement.assignment

import core.language.node.NodeShape
import deltas.expression.bitwise.BitwiseShiftLeftDelta

object BitwiseShiftLeftAssignmentDelta extends OperatorWithAssignmentDelta {

  override def operatorDelta = BitwiseShiftLeftDelta

  object Shape extends NodeShape
  override val shape = Shape
}
