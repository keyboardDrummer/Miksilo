package deltas.expression.bitwise

import core.language.node.NodeShape
import deltas.expression.{ExpressionDelta, LeftAssociativeBinaryOperatorDelta}

object BitwiseOrDelta extends LeftAssociativeBinaryOperatorDelta {
  override def operatorGrammarKey = ExpressionDelta.FirstPrecedenceGrammar

  override def keyword = "|"
  object Shape extends NodeShape
  override def shape = Shape
}
