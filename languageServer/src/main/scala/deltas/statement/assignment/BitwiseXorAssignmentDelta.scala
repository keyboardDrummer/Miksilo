package deltas.statement.assignment

import core.language.node.NodeShape
import deltas.expression.bitwise.{BitwiseAndDelta, BitwiseShiftLeftDelta, BitwiseShiftRightDelta, BitwiseXorDelta}
import deltas.expression.multiplicative.{DivideDelta, MultiplyDelta}

object BitwiseXorAssignmentDelta extends OperatorWithAssignmentDelta {

  override def operatorDelta = BitwiseXorDelta

  object Shape extends NodeShape
  override val shape = Shape
}









