package deltas.expression.relational

import core.deltas.grammars.LanguageGrammars
import core.deltas.path.NodePath
import core.deltas.{Contract, DeltaWithGrammar}
import core.language.node._
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.types.{IntTypeDelta, TypeSkeleton}
import deltas.expression.{ExpressionDelta, ExpressionInstance}
import deltas.javac.types.BooleanTypeDelta

object ComparisonOperatorDelta {

  implicit class ComparisonOperator[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def left: T = node(Left).asInstanceOf[T]
    def right: T = node(Right).asInstanceOf[T]
  }

  object Left extends NodeField

  object Right extends NodeField

}

trait ComparisonOperatorDelta extends DeltaWithGrammar with ExpressionInstance {
  import ComparisonOperatorDelta._

  def neww(first: Node, second: Node) = new Node(Shape, Left -> first, Right -> second)

  val shape: NodeShape = Shape

  object Shape extends NodeShape

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
    //TODO add a check for first and secondType. Share code with other comparisons.
    val firstType = ExpressionDelta.getType(compilation, builder, expression.left, parentScope)
    val secondType = ExpressionDelta.getType(compilation, builder, expression.right, parentScope)
    builder.typesAreEqual(firstType, secondType)
    builder.typesAreEqual(_type, BooleanTypeDelta.constraintType)
  }

  override def getType(lessThan: NodePath, compilation: Compilation): Node = {
    val getType = ExpressionDelta.getType(compilation)
    val firstType = getType(lessThan.left)
    val secondType = getType(lessThan.right)
    TypeSkeleton.checkAssignableTo(compilation)(IntTypeDelta.intType, firstType)
    TypeSkeleton.checkAssignableTo(compilation)(IntTypeDelta.intType, secondType)
    BooleanTypeDelta.booleanType
  }

  def keyword: String

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit =  {
    import grammars._

    val relationalGrammar = find(AddRelationalPrecedenceDelta.RelationalExpressionGrammar)
    val parseLessThan = ((relationalGrammar.as(Left) ~~< keyword) ~~ relationalGrammar.as(Right)).asNode(Shape)
    relationalGrammar.addAlternative(parseLessThan)
  }

  override def dependencies: Set[Contract] = Set(AddRelationalPrecedenceDelta)
}
