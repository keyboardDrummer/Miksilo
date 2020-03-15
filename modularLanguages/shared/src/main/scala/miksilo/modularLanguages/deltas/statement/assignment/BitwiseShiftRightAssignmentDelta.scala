package miksilo.modularLanguages.deltas.statement.assignment

import miksilo.modularLanguages.core.node.NodeShape
import miksilo.modularLanguages.deltas.expression.bitwise.BitwiseShiftRightDelta

object BitwiseShiftRightAssignmentDelta extends OperatorWithAssignmentDelta {

  override def operatorDelta = BitwiseShiftRightDelta

  object Shape extends NodeShape
  override val shape = Shape
}
