package deltas.expression

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node._

trait LeftAssociativeBinaryOperatorDelta extends DeltaWithGrammar {
  object Shape extends NodeShape
  object Left extends NodeField
  object Right extends NodeField

  override def dependencies = Set(ExpressionDelta)

  implicit class BinaryOperator[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def left: T = node(Left).asInstanceOf[T]
    def left_=(value: T): Unit = node(Left) = value

    def right: T = node(Right).asInstanceOf[T]
    def right_=(value: T): Unit = node(Right) = value
  }

  def operatorGrammarKey: GrammarKey
  def keyword: String

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val operatorGrammar = find(operatorGrammarKey)
    val withoutOperator = operatorGrammar.inner
    val parseSubtraction = operatorGrammar.as(Left) ~~< keyword ~~ withoutOperator.as(Right) asNode Shape
    operatorGrammar.addAlternative(parseSubtraction)
  }

  override def description: String = s"Adds the $keyword operator."
}

