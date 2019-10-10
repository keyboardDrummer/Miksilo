package deltas.expression.bitwise

import core.language.node.NodeShape
import deltas.expression.{BinaryOperatorDelta, ExpressionDelta, LeftAssociativeBinaryOperatorDelta}

object BitwiseOrDelta extends BinaryOperatorDelta {
  override def precedenceGrammarKey = ExpressionDelta.FirstPrecedenceGrammar

  override def keyword = "|"
  object Shape extends NodeShape
  override def shape = Shape
}
