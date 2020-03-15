package miksilo.modularLanguages.deltas.expression.multiplicative

import miksilo.modularLanguages.core.node.NodeShape
import miksilo.modularLanguages.deltas.expression.LeftAssociativeBinaryOperatorDelta

object DivideDelta extends LeftAssociativeBinaryOperatorDelta {
  override def precedenceGrammarKey = MultiplicativePrecedenceDelta.Grammar

  override def keyword = "/"

  object Shape extends NodeShape
  override def shape = Shape
}
