package miksilo.modularLanguages.deltas.expression.bitwise

import miksilo.modularLanguages.core.node.NodeShape
import miksilo.modularLanguages.deltas.expression.{BinaryOperatorDelta, ExpressionDelta, LeftAssociativeBinaryOperatorDelta}

object BitwiseAndDelta extends BinaryOperatorDelta {
  override def precedenceGrammarKey = ExpressionDelta.FirstPrecedenceGrammar

  override def keyword = "&"
  object Shape extends NodeShape
  override def shape = Shape
}
