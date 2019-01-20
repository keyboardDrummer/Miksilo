package deltas.expression.bitwise

import core.language.node.NodeShape
import deltas.expression.{ExpressionDelta, LeftAssociativeBinaryOperatorDelta}

object BitwiseShiftRightDelta extends LeftAssociativeBinaryOperatorDelta {
  override def precedenceGrammarKey = ExpressionDelta.FirstPrecedenceGrammar

  override def keyword = ">>"
  object Shape extends NodeShape
  override def shape = Shape
}
