package deltas.expression.multiplicative

import core.language.node.NodeShape
import deltas.expression.LeftAssociativeBinaryOperatorDelta

object DivideDelta extends LeftAssociativeBinaryOperatorDelta {
  override def precedenceGrammarKey = MultiplicativePrecedenceDelta.Grammar

  override def keyword = "/"

  object Shape extends NodeShape
  override def shape = Shape
}
