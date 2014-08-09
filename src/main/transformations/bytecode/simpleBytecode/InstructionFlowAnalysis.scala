package transformations.bytecode.simpleBytecode

import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.{ByteCodeSkeleton, LabelledJumps}
import util.DataFlowAnalysis

abstract class InstructionFlowAnalysis[State](instructions: Seq[MetaObject], state: TransformationState)
  extends DataFlowAnalysis[Int, State] {

  val labels = instructions.zipWithIndex.filter(i => i._1.clazz == LabelledJumps.LabelKey)
    .map(p => (LabelledJumps.getLabelName(p._1), p._2)).toMap

  override def getOutgoingNodes(instructionIndex: Int): Set[Int] = {
    val instruction = instructions(instructionIndex)

    val jumpBehavior = ByteCodeSkeleton.getState(state).jumpBehaviorRegistry(instruction.clazz)
    var result = Set.empty[Int]
    if (jumpBehavior.movesToNext)
      result += instructionIndex + 1

    if (jumpBehavior.hasJumpInFirstArgument)
      result += labels(getInstructionArguments(instruction)(0).asInstanceOf[String])

    result
  }
}
