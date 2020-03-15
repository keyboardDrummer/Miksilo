package miksilo.modularLanguages.deltas.statement.assignment

import miksilo.modularLanguages.core.node.NodeShape
import miksilo.modularLanguages.deltas.expression.multiplicative.MultiplyDelta

object MultiplyAssignmentDelta extends OperatorWithAssignmentDelta {

  override def operatorDelta = MultiplyDelta

  object Shape extends NodeShape
  override val shape = Shape
}
