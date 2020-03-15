package miksilo.modularLanguages.deltas.expression.multiplicative

import miksilo.modularLanguages.core.node.NodeShape
import miksilo.modularLanguages.deltas.expression.{BinaryOperatorDelta, LeftAssociativeBinaryOperatorDelta}

object MultiplyDelta extends BinaryOperatorDelta {
  override def precedenceGrammarKey = MultiplicativePrecedenceDelta.Grammar

  override def keyword = "*"

  object Shape extends NodeShape
  override def shape = Shape
}
