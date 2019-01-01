package deltas.javac.expressions.relational

object EqualsComparisonDelta extends ComparisonOperatorDelta {

  override def description: String = "Adds the == operator."

  override def keyword = "=="
}
