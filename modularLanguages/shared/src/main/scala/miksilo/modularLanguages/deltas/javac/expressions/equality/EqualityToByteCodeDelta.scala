package miksilo.modularLanguages.deltas.javac.expressions.equality

import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.Compilation
import miksilo.modularLanguages.core.node._
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.longs.CompareLongDelta
import miksilo.modularLanguages.deltas.bytecode.extraBooleanInstructions.{IntegerEqualsInstructionDelta, NotInstructionDelta}
import miksilo.modularLanguages.deltas.bytecode.types.{IntTypeDelta, LongTypeDelta, TypeSkeleton}
import miksilo.modularLanguages.deltas.expression.ExpressionDelta
import miksilo.modularLanguages.deltas.expression.LeftAssociativeBinaryOperatorDelta.BinaryOperator
import miksilo.modularLanguages.deltas.expression.relational.EqualsComparisonDelta
import miksilo.modularLanguages.deltas.javac.expressions.{ConvertsToByteCodeDelta, ToByteCodeSkeleton}

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

