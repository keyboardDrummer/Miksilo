package miksilo.modularLanguages.deltas.statement.assignment

import miksilo.modularLanguages.core.node.NodeShape
import miksilo.modularLanguages.deltas.expression.additive.AdditionDelta

object AddAssignmentDelta extends OperatorWithAssignmentDelta {

  object Shape extends NodeShape

  override def operatorDelta = AdditionDelta

  override val shape = Shape
}
