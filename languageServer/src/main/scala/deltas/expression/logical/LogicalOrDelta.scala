package deltas.expression.logical

import core.language.node.NodeShape
import deltas.expression.{ExpressionDelta, LeftAssociativeBinaryOperatorDelta}

object LogicalOrDelta extends LeftAssociativeBinaryOperatorDelta {
  override def operatorGrammarKey = ExpressionDelta.FirstPrecedenceGrammar

  override def keyword = "||"

  object Shape extends NodeShape
  override def shape = Shape
}


