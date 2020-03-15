package miksilo.modularLanguages.deltas.statement.assignment

import miksilo.modularLanguages.core.node.NodeShape
import miksilo.modularLanguages.deltas.expression.bitwise.BitwiseOrDelta

object BitwiseOrAssignmentDelta extends OperatorWithAssignmentDelta {

  override def operatorDelta = BitwiseOrDelta

  object Shape extends NodeShape
  override val shape = Shape
}
