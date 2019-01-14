package deltas.expression

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.language.Language
import core.language.node._

object LeftAssociativeBinaryOperatorDelta {
  object Left extends NodeField
  object Right extends NodeField

  implicit class BinaryOperator[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def left: T = node(Left).asInstanceOf[T]
    def left_=(value: T): Unit = node(Left) = value

    def right: T = node(Right).asInstanceOf[T]
    def right_=(value: T): Unit = node(Right) = value
  }
}

trait LeftAssociativeBinaryOperatorDelta extends DeltaWithGrammar {
  import LeftAssociativeBinaryOperatorDelta._

  override def dependencies = Set(ExpressionDelta)

  def neww(left: Node, right: Node) = shape.create(Left -> left, Right -> right)

  def shape: NodeShape
  def operatorGrammarKey: GrammarKey
  def keyword: String

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val operatorGrammar = find(operatorGrammarKey)
    val withoutOperator = operatorGrammar.inner
    val parseSubtraction = operatorGrammar.as(Left) ~~< keyword ~~ withoutOperator.as(Right) asNode shape
    operatorGrammar.addAlternative(parseSubtraction)
  }

  override def description: String = s"Adds the $keyword operator."
}

