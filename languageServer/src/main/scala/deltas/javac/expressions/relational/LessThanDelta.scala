package deltas.javac.expressions.relational

import core.deltas._
import core.deltas.path.NodePath
import core.language.Compilation
import core.language.node._
import deltas.bytecode.extraBooleanInstructions.LessThanInstructionDelta
import deltas.javac.expressions.{ConvertsToByteCodeDelta, ToByteCodeSkeleton}

object LessThanDelta extends DeltaWithGrammar with ComparisonOperatorDelta with ConvertsToByteCodeDelta {

  override def description: String = "Adds the < operator."

  override def dependencies: Set[Contract] = super.dependencies ++ Set(LessThanInstructionDelta)

  override def toByteCode(lessThan: NodePath, compilation: Compilation): Seq[Node] = { //TODO move into comparisonOperatorDelta.
    val toInstructions = ToByteCodeSkeleton.getToInstructions(compilation)
    val firstInstructions = toInstructions(lessThan.left)
    val secondInstructions = toInstructions(lessThan.right)
    firstInstructions ++ secondInstructions ++ Seq(LessThanInstructionDelta.lessThanInstruction)
  }

  override def keyword = "<"
}
