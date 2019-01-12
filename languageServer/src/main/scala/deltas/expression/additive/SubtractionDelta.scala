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
import deltas.expression.{ExpressionDelta, ExpressionInstance, LeftAssociativeBinaryOperatorDelta}

object SubtractionDelta extends LeftAssociativeBinaryOperatorDelta with ExpressionInstance {
  object Shape extends NodeShape

  override def dependencies: Set[Contract] = Set(AdditivePrecedenceDelta)

  override val shape = Shape

  override def getType(expression: NodePath, compilation: Compilation): Node = {
    val subtraction: BinaryOperator[NodePath] = expression
    val getType = ExpressionDelta.getType(compilation)
    val firstType = getType(subtraction.left)
    val secondType = getType(subtraction.right)
    TypeSkeleton.checkAssignableTo(compilation)(IntTypeDelta.intType, firstType)
    TypeSkeleton.checkAssignableTo(compilation)(IntTypeDelta.intType, secondType)
    IntTypeDelta.intType
  }

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
    val left = expression.left
    val right = expression.right
    additionOrSubtractionConstraints(compilation, builder, _type, parentScope, left, right)
  }

  override def operatorGrammarKey = AdditivePrecedenceDelta.Grammar

  override def keyword = "-"
}
