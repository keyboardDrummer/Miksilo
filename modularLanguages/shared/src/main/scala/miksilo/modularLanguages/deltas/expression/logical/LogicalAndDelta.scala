package miksilo.modularLanguages.deltas.expression.logical

import miksilo.modularLanguages.core.node.NodeShape
import miksilo.modularLanguages.deltas.expression.{BinaryOperatorDelta, ExpressionDelta, LeftAssociativeBinaryOperatorDelta}

object LogicalAndDelta extends BinaryOperatorDelta {
  override def precedenceGrammarKey = ExpressionDelta.FirstPrecedenceGrammar

  override def keyword = "&&"

  object Shape extends NodeShape
  override def shape = Shape
}
