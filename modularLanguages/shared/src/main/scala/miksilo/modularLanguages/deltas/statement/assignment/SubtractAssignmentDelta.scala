package miksilo.modularLanguages.deltas.statement.assignment

import miksilo.modularLanguages.core.node.NodeShape
import miksilo.modularLanguages.deltas.expression.additive.SubtractionDelta

object SubtractAssignmentDelta extends OperatorWithAssignmentDelta {

  override def operatorDelta = SubtractionDelta

  object Shape extends NodeShape
  override val shape = Shape
}
