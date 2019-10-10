package deltas.javac.expressions.equality

import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node._
import deltas.bytecode.coreInstructions.longs.CompareLongDelta
import deltas.bytecode.extraBooleanInstructions.{IntegerEqualsInstructionDelta, NotInstructionDelta}
import deltas.bytecode.types.{IntTypeDelta, LongTypeDelta, TypeSkeleton}
import deltas.expression.ExpressionDelta
import deltas.expression.LeftAssociativeBinaryOperatorDelta.BinaryOperator
import deltas.expression.relational.EqualsComparisonDelta
import deltas.javac.expressions.{ConvertsToByteCodeDelta, ToByteCodeSkeleton}

object EqualityToByteCodeDelta extends ConvertsToByteCodeDelta {

  override def toByteCode(path: NodePath, compilation: Compilation): Seq[Node] = {
    val binaryOperator: BinaryOperator[NodePath] = path
    val first = binaryOperator.left
    val second = binaryOperator.right
    val toInstructions = ToByteCodeSkeleton.getToInstructions(compilation)
    val nodeType = ExpressionDelta.cachedNodeType(compilation, first)
    val inputType = TypeSkeleton.toStackType(nodeType, compilation)
    val equalityInstructions: Seq[Node] = inputType.shape match {
      case LongTypeDelta.Shape => Seq(CompareLongDelta.compareLong, NotInstructionDelta.not)
      case IntTypeDelta.Shape => Seq(IntegerEqualsInstructionDelta.equals)
    }
    toInstructions(first) ++ toInstructions(second) ++ equalityInstructions
  }

  override def description = "Converts the == operator to bytecode"

  override def dependencies = Set(EqualsComparisonDelta)

  override def shape = EqualsComparisonDelta.Shape
}

