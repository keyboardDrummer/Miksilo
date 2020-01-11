package deltas.statement.assignment

import core.language.node.NodeShape
import deltas.expression.bitwise.BitwiseAndDelta

object BitwiseAndAssignmentDelta extends OperatorWithAssignmentDelta {

  override def operatorDelta = BitwiseAndDelta

  object Shape extends NodeShape
  override val shape = Shape
}
