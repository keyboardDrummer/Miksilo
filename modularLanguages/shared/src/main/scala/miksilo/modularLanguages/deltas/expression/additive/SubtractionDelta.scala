package miksilo.modularLanguages.deltas.expression.additive

import miksilo.modularLanguages.core.deltas.Contract
import miksilo.modularLanguages.core.node._
import miksilo.modularLanguages.deltas.expression.{ExpressionInstance, LeftAssociativeBinaryOperatorDelta}

object SubtractionDelta extends LeftAssociativeBinaryOperatorDelta with ExpressionInstance {
  object Shape extends NodeShape

  override def dependencies: Set[Contract] = Set(AdditivePrecedenceDelta)

  override val shape = Shape

  override def precedenceGrammarKey = AdditivePrecedenceDelta.Grammar

  override def keyword = "-"
}
