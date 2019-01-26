package deltas.expression.additive

import core.language.node._
import deltas.expression.{ExpressionInstance, LeftAssociativeBinaryOperatorDelta}

object AdditionDelta extends LeftAssociativeBinaryOperatorDelta with ExpressionInstance {

  override def description: String = "Adds the + operator."

  val shape = Shape

  object Shape extends NodeShape

  override def precedenceGrammarKey = AdditivePrecedenceDelta.Grammar

  override def keyword = "+"
}
