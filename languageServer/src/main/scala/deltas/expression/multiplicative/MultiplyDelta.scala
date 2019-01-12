package deltas.expression.multiplicative

import deltas.expression.LeftAssociativeBinaryOperatorDelta

object MultiplyDelta extends LeftAssociativeBinaryOperatorDelta {
  override def operatorGrammarKey = MultiplicativePrecedenceDelta.Grammar

  override def keyword = "*"
}
