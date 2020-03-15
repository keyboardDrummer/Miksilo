package miksilo.modularLanguages.deltas.expression

import miksilo.modularLanguages.core.deltas.DeltaWithGrammar
import miksilo.modularLanguages.core.deltas.grammars.LanguageGrammars
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.core.node._
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.Type

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

trait BinaryOperatorDelta extends DeltaWithGrammar with ExpressionInstance {
  import LeftAssociativeBinaryOperatorDelta._

  override def dependencies = Set(ExpressionDelta)

  def neww(left: Node, right: Node) = shape.create(Left -> left, Right -> right)

  def shape: NodeShape
  def precedenceGrammarKey: GrammarKey
  def keyword: String

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val precedenceGrammar = find(precedenceGrammarKey)
    val operator = precedenceGrammar.as(Left) ~~< keyword ~~ precedenceGrammar.as(Right) asLabelledNode shape
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

trait LeftAssociativeBinaryOperatorDelta extends BinaryOperatorDelta {
  import LeftAssociativeBinaryOperatorDelta._

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit = {
    import grammars._
    val precedenceGrammar = find(precedenceGrammarKey)
    val withoutOperator = precedenceGrammar.inner
    val operator = precedenceGrammar.as(Left) ~~< keyword ~~ withoutOperator.as(Right) asLabelledNode shape
    precedenceGrammar.addAlternative(operator)
  }
}

