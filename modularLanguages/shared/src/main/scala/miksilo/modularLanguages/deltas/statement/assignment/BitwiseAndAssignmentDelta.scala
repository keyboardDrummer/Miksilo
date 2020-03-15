package miksilo.modularLanguages.deltas.statement.assignment

import miksilo.modularLanguages.core.node.NodeShape
import miksilo.modularLanguages.deltas.expression.bitwise.BitwiseAndDelta

object BitwiseAndAssignmentDelta extends OperatorWithAssignmentDelta {

  override def operatorDelta = BitwiseAndDelta

  object Shape extends NodeShape
  override val shape = Shape
}
