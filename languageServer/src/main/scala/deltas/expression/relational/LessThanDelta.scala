package deltas.expression.relational

import core.language.node.NodeShape

object LessThanDelta extends ComparisonOperatorDelta {

  override def description: String = "Adds the < operator."

  override def keyword = "<"

  object Shape extends NodeShape

  override val shape = Shape
}
