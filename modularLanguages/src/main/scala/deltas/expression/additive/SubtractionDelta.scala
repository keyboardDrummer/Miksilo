package deltas.expression.additive

import core.deltas.Contract
import core.language.node._
import deltas.expression.{ExpressionInstance, LeftAssociativeBinaryOperatorDelta}

object SubtractionDelta extends LeftAssociativeBinaryOperatorDelta with ExpressionInstance {
  object Shape extends NodeShape

  override def dependencies: Set[Contract] = Set(AdditivePrecedenceDelta)

  override val shape = Shape

  override def precedenceGrammarKey = AdditivePrecedenceDelta.Grammar

  override def keyword = "-"
}
