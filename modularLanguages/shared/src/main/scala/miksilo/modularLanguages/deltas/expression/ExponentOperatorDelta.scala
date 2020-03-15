package miksilo.modularLanguages.deltas.expression

import miksilo.modularLanguages.core.node.NodeShape

object ExponentOperatorDelta extends LeftAssociativeBinaryOperatorDelta {

  object Shape extends NodeShape

  override def shape = Shape

  override def precedenceGrammarKey = ExpressionDelta.FirstPrecedenceGrammar

  override def keyword = "**"
}
