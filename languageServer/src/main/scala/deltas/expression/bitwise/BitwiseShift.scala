package deltas.expression.bitwise

import deltas.expression.{ExpressionDelta, LeftAssociativeBinaryOperatorDelta}

object BitwiseShiftLeftDelta extends LeftAssociativeBinaryOperatorDelta {
  override def operatorGrammarKey = ExpressionDelta.FirstPrecedenceGrammar

  override def keyword = "<<"
}

object BitwiseShiftRightDelta extends LeftAssociativeBinaryOperatorDelta {
  override def operatorGrammarKey = ExpressionDelta.FirstPrecedenceGrammar

  override def keyword = ">>"
}

object BitwiseAndDelta extends LeftAssociativeBinaryOperatorDelta {
  override def operatorGrammarKey = ExpressionDelta.FirstPrecedenceGrammar

  override def keyword = "&"
}

object BitwiseXorDelta extends LeftAssociativeBinaryOperatorDelta {
  override def operatorGrammarKey = ExpressionDelta.FirstPrecedenceGrammar

  override def keyword = "^"
}

object BitwiseOrDelta extends LeftAssociativeBinaryOperatorDelta {
  override def operatorGrammarKey = ExpressionDelta.FirstPrecedenceGrammar

  override def keyword = "|"
}