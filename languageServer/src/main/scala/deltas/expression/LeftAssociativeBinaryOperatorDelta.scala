package deltas.expression

import core.deltas.DeltaWithGrammar
import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.language.{Compilation, Language}
import core.language.node._
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type

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

trait LeftAssociativeBinaryOperatorDelta extends DeltaWithGrammar with ExpressionInstance {
  import LeftAssociativeBinaryOperatorDelta._

  override def dependencies = Set(ExpressionDelta)

  def neww(left: Node, right: Node) = shape.create(Left -> left, Right -> right)

  def shape: NodeShape
  def precedenceGrammarKey: GrammarKey
  def keyword: String

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val precedenceGrammar = find(precedenceGrammarKey)
    val withoutOperator = precedenceGrammar.inner
    val operator = precedenceGrammar.as(Left) ~~< keyword ~~ withoutOperator.as(Right) asLabelledNode shape
    precedenceGrammar.addAlternative(operator)
  }

  override def description: String = s"Adds the $keyword operator."

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
    val left = expression.left
    val right = expression.right
    mergeOperatorConstraints(compilation, builder, _type, parentScope, left, right)
  }

  def mergeOperatorConstraints(compilation: Compilation, builder: ConstraintBuilder, _type: Type, parentScope: Scope, left: NodePath, right: NodePath): Unit = {
    val firstType = ExpressionDelta.getType(compilation, builder, left, parentScope)
    val secondType = ExpressionDelta.getType(compilation, builder, right, parentScope)
    builder.typesAreEqual(firstType, secondType)
    builder.typesAreEqual(_type, firstType)
  }
}

