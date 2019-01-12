package deltas.statement.assignment

import core.language.node.NodeShape
import deltas.expression.additive.SubtractionDelta

object SubtractAssignmentDelta extends OperatorWithAssignmentDelta {

  override def operatorDelta = SubtractionDelta

  object Shape extends NodeShape
  override val shape = Shape
}
