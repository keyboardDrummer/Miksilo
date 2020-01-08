package deltas.expression.multiplicative

import core.language.node.NodeShape
import deltas.expression.{BinaryOperatorDelta, LeftAssociativeBinaryOperatorDelta}

object MultiplyDelta extends BinaryOperatorDelta {
  override def precedenceGrammarKey = MultiplicativePrecedenceDelta.Grammar

  override def keyword = "*"

  object Shape extends NodeShape
  override def shape = Shape
}
