package deltas.expression.relational

object EqualsComparisonDelta extends ComparisonOperatorDelta {

  override def description: String = "Adds the == operator."

  override def keyword = "=="
}
