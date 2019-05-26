package deltas.expression.logical

import core.language.node.NodeShape
import deltas.expression.{BinaryOperatorDelta, ExpressionDelta, LeftAssociativeBinaryOperatorDelta}

object LogicalOrDelta extends BinaryOperatorDelta {
  override def precedenceGrammarKey = ExpressionDelta.FirstPrecedenceGrammar

  override def keyword = "||"

  object Shape extends NodeShape
  override def shape = Shape
}


