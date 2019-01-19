package deltas.expression.additive

import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node._
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.types.{IntTypeDelta, LongTypeDelta, TypeSkeleton}
import deltas.expression.LeftAssociativeBinaryOperatorDelta.BinaryOperator
import deltas.expression.{ExpressionDelta, JavaExpressionInstance, LeftAssociativeBinaryOperatorDelta}

object AdditionDelta extends LeftAssociativeBinaryOperatorDelta with JavaExpressionInstance {

  override def description: String = "Adds the + operator."

  val shape = Shape

  override def getType(expression: NodePath, compilation: Compilation): Node = {
    val getType = ExpressionDelta.getType(compilation)
    val operator: BinaryOperator[NodePath] = expression
    val firstType = getType(operator.left)
    val secondType = getType(operator.right)
    firstType match
    {
      case x if x == IntTypeDelta.intType =>
        TypeSkeleton.checkAssignableTo(compilation)(IntTypeDelta.intType, firstType)
        TypeSkeleton.checkAssignableTo(compilation)(IntTypeDelta.intType, secondType)
        IntTypeDelta.intType
      case x if x == LongTypeDelta.longType =>
        TypeSkeleton.checkAssignableTo(compilation)(LongTypeDelta.longType, firstType)
        TypeSkeleton.checkAssignableTo(compilation)(LongTypeDelta.longType, secondType)
        LongTypeDelta.longType
      case _ => throw new NotImplementedError()
    }
  }

  object Shape extends NodeShape

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
    val left = expression.left
    val right = expression.right
    additionOrSubtractionConstraints(compilation, builder, _type, parentScope, left, right)
  }

  def additionOrSubtractionConstraints(compilation: Compilation, builder: ConstraintBuilder, _type: Type, parentScope: Scope, left: NodePath, right: NodePath): Unit = {
    val firstType = ExpressionDelta.getType(compilation, builder, left, parentScope)
    val secondType = ExpressionDelta.getType(compilation, builder, right, parentScope)
    builder.typesAreEqual(firstType, secondType)
    builder.typesAreEqual(_type, firstType)

    // TODO add constraint that once _type is resolved, verifies that it's one of the allowed types.
  }

  override def operatorGrammarKey = AdditivePrecedenceDelta.Grammar

  override def keyword = "+"
}
