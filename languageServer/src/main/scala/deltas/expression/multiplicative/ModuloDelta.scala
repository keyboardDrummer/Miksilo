package deltas.expression.multiplicative

import deltas.expression.LeftAssociativeBinaryOperatorDelta

object ModuloDelta extends LeftAssociativeBinaryOperatorDelta {
  override def operatorGrammarKey = MultiplicativePrecedenceDelta.Grammar

  override def keyword = "%"
}
