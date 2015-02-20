package transformations.bytecode.simpleBytecode

import core.transformation.MetaObject
import transformations.bytecode.ByteCodeSkeleton.JumpBehavior

class LocalTypeAnalysis(instructions: Seq[MetaObject], getVariableUpdates: MetaObject => Map[Int, MetaObject], getJumpBehavior: Any => JumpBehavior)
  extends InstructionFlowAnalysis[Map[Int, MetaObject]](instructions, getJumpBehavior) {

  override def combineState(first: Map[Int, MetaObject], second: Map[Int, MetaObject]): Option[Map[Int, MetaObject]] = {
    if (first == second)
      return None

    // TODO -- maybe we should do some merging here. Check how JavaC behaves.
    throw new TargetInstructionEnteredWithDifferentLayouts(first, second)
  }

  override def updateState(state: Map[Int, MetaObject], instructionIndex: Int): Map[Int, MetaObject] = {
    val instruction = instructions(instructionIndex)
    val updates = getVariableUpdates(instruction)
    updates ++ state
  }

  case class StackDoesNotFitInstructionInput(instruction: Any, inputTypes: Seq[Any], stack: Seq[Any]) extends RuntimeException {
    override def toString = s"StackDoesNotFitInstructionInput: instruction= $instruction; inputTypes= $inputTypes; stack= $stack"
  }

  case class TargetInstructionEnteredWithDifferentLayouts(first: Map[Int, MetaObject], second: Map[Int, MetaObject]) extends RuntimeException

}
