package deltas.expression.multiplicative

import deltas.expression.LeftAssociativeBinaryOperatorDelta

object DivideDelta extends LeftAssociativeBinaryOperatorDelta {
  override def operatorGrammarKey = MultiplicativePrecedenceDelta.Grammar

  override def keyword = "/"
}
