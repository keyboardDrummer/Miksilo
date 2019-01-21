package deltas.expression.additive

import core.deltas.Contract
import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node._
import deltas.bytecode.types.{IntTypeDelta, TypeSkeleton}
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

  override def precedenceGrammarKey = AdditivePrecedenceDelta.Grammar

  override def keyword = "-"
}
