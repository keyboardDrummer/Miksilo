package deltas.javac.expressions.relational

import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node._
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.types.{IntTypeDelta, TypeSkeleton}
import deltas.expressions.ExpressionDelta
import deltas.javac.expressions.ExpressionInstance
import deltas.javac.types.BooleanTypeDelta

trait ComparisonOperatorDelta extends ExpressionInstance {

  implicit class ComparisonOperator[T <: NodeLike](val node: T) extends NodeWrapper[T] {
    def left: T = node(Left).asInstanceOf[T]
    def right: T = node(Right).asInstanceOf[T]
  }

  def neww(first: Node, second: Node) = new Node(Shape, Left -> first, Right -> second)

  val shape: NodeShape = Shape

  object Shape extends NodeShape

  object Left extends NodeField

  object Right extends NodeField

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

}
