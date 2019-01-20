package deltas.expression.relational

import core.deltas.Contract
import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node._
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type
import deltas.bytecode.types.{IntTypeDelta, TypeSkeleton}
import deltas.expression.{ExpressionDelta, JavaExpressionInstance, LeftAssociativeBinaryOperatorDelta}
import deltas.javac.types.BooleanTypeDelta

trait ComparisonOperatorDelta extends LeftAssociativeBinaryOperatorDelta with JavaExpressionInstance {
  import LeftAssociativeBinaryOperatorDelta._

  val shape: NodeShape

  override def precedenceGrammarKey = AddRelationalPrecedenceDelta.RelationalExpressionGrammar

  override def constraints(compilation: Compilation, builder: ConstraintBuilder, expression: NodePath, _type: Type, parentScope: Scope): Unit = {
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

  override def dependencies: Set[Contract] = Set(AddRelationalPrecedenceDelta)
}
