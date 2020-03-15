package miksilo.modularLanguages.deltas.expression.relational

import miksilo.modularLanguages.core.node.NodeShape

object EqualsComparisonDelta extends ComparisonOperatorDelta {

  override def description: String = "Adds the == operator."

  override def keyword = "=="

  object Shape extends NodeShape

  override val shape = Shape
}
