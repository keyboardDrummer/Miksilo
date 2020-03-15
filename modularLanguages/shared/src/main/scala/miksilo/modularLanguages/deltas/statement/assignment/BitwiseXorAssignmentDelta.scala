package miksilo.modularLanguages.deltas.statement.assignment

import miksilo.modularLanguages.core.node.NodeShape
import miksilo.modularLanguages.deltas.expression.bitwise.{BitwiseAndDelta, BitwiseShiftLeftDelta, BitwiseShiftRightDelta, BitwiseXorDelta}
import miksilo.modularLanguages.deltas.expression.multiplicative.{DivideDelta, MultiplyDelta}

object BitwiseXorAssignmentDelta extends OperatorWithAssignmentDelta {

  override def operatorDelta = BitwiseXorDelta

  object Shape extends NodeShape
  override val shape = Shape
}









