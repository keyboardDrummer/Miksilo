package miksilo.modularLanguages.deltas.statement.assignment

import miksilo.modularLanguages.core.node.NodeShape
import miksilo.modularLanguages.deltas.expression.multiplicative.DivideDelta

object DivideAssignmentDelta extends OperatorWithAssignmentDelta {

  override def operatorDelta = DivideDelta

  object Shape extends NodeShape
  override val shape = Shape
}
