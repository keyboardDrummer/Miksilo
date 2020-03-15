package miksilo.modularLanguages.deltas.statement.assignment

import miksilo.modularLanguages.core.node.NodeShape
import miksilo.modularLanguages.deltas.expression.bitwise.BitwiseShiftLeftDelta

object BitwiseShiftLeftAssignmentDelta extends OperatorWithAssignmentDelta {

  override def operatorDelta = BitwiseShiftLeftDelta

  object Shape extends NodeShape
  override val shape = Shape
}
