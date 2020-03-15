package miksilo.modularLanguages.deltas.expression.additive

import miksilo.modularLanguages.core.node._
import miksilo.modularLanguages.deltas.expression.{BinaryOperatorDelta, ExpressionInstance, LeftAssociativeBinaryOperatorDelta}

object AdditionDelta extends BinaryOperatorDelta with ExpressionInstance {

  override def description: String = "Adds the + operator."

  val shape = Shape

  object Shape extends NodeShape

  override def precedenceGrammarKey = AdditivePrecedenceDelta.Grammar

  override def keyword = "+"
}
