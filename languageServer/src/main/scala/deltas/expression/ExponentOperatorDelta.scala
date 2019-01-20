package deltas.expression

import core.language.node.NodeShape

object ExponentOperatorDelta extends LeftAssociativeBinaryOperatorDelta {

  object Shape extends NodeShape

  override def shape = Shape

  override def precedenceGrammarKey = ExpressionDelta.FirstPrecedenceGrammar

  override def keyword = "**"
}
