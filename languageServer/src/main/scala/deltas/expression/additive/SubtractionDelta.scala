package deltas.expression.additive

import core.deltas.Contract
import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node._
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.types.{IntTypeDelta, TypeSkeleton}
import deltas.expression.additive.AdditionDelta.additionOrSubtractionConstraints
import deltas.expression.{ExpressionDelta, JavaExpressionInstance, LeftAssociativeBinaryOperatorDelta}

object SubtractionDelta extends LeftAssociativeBinaryOperatorDelta with JavaExpressionInstance {
  object Shape extends NodeShape

  override def dependencies: Set[Contract] = Set(AdditivePrecedenceDelta)

  override val shape = Shape

  override def getType(expression: NodePath, compilation: Compilation): Node = {
    val subtraction: LeftAssociativeBinaryOperatorDelta.BinaryOperator[NodePath] = expression
    val getType = ExpressionDelta.getType(compilation)
    val firstType = getType(subtraction.left)
    val secondType = getType(subtraction.right)
    TypeSkeleton.checkAssignableTo(compilation)(IntTypeDelta.intType, firstType)
    TypeSkeleton.checkAssignableTo(compilation)(IntTypeDelta.intType, secondType)
    IntTypeDelta.intType
  }

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
    val subtraction: LeftAssociativeBinaryOperatorDelta.BinaryOperator[NodePath] = expression
    val left = subtraction.left
    val right = subtraction.right
    additionOrSubtractionConstraints(compilation, builder, _type, parentScope, left, right)
  }

  override def operatorGrammarKey = AdditivePrecedenceDelta.Grammar

  override def keyword = "-"
}
