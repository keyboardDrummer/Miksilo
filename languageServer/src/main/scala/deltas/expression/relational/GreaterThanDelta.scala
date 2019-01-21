package deltas.expression.relational

import core.language.node.NodeShape

object GreaterThanDelta extends ComparisonOperatorDelta {

  override def description: String = "Adds the > operator."

  override def keyword = ">"

  object Shape extends NodeShape

  override val shape = Shape
}
