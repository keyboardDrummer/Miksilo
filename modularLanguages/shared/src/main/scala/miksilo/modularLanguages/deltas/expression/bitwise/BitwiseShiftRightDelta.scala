package miksilo.modularLanguages.deltas.expression.bitwise

import miksilo.modularLanguages.core.node.NodeShape
import miksilo.modularLanguages.deltas.expression.{ExpressionDelta, LeftAssociativeBinaryOperatorDelta}

object BitwiseShiftRightDelta extends LeftAssociativeBinaryOperatorDelta {
  override def precedenceGrammarKey = ExpressionDelta.FirstPrecedenceGrammar

  override def keyword = ">>"
  object Shape extends NodeShape
  override def shape = Shape
}
