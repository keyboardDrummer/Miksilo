package transformations.bytecode.simpleBytecode

import core.exceptions.BadInputException
import core.transformation.MetaObject
import transformations.bytecode.ByteCodeSkeleton.JumpBehavior
import transformations.bytecode.coreInstructions.InstructionSignature

class StackLayoutAnalysis(instructions: Seq[MetaObject], getSignature: (Seq[MetaObject], MetaObject) => InstructionSignature,
                          getJumpBehavior: Any => JumpBehavior)
  extends InstructionFlowAnalysis[Seq[MetaObject]](instructions, getJumpBehavior) {

  case class StackDoesNotFitInstructionInput(instruction: Any, inputTypes: Seq[Any], stack: Seq[Any]) extends RuntimeException {
    override def toString = s"StackDoesNotFitInstructionInput: instruction= $instruction; inputTypes= $inputTypes; stack= $stack"
  }

  case class TargetInstructionEnteredWithDifferentLayouts(first: Seq[MetaObject], second: Seq[MetaObject]) extends RuntimeException
  {
    override def toString = s"TargetInstructionEnteredWithDifferentLayouts, first:=$first, second:= $second"
  }

  override def combineState(first: Seq[MetaObject], second: Seq[MetaObject]): Option[Seq[MetaObject]] = {
    if (first == second)
      return None

    throw new TargetInstructionEnteredWithDifferentLayouts(first, second)
  }

  override def updateState(state: Seq[MetaObject], instructionIndex: Int): Seq[MetaObject] = {
    try
    {
      val instruction = instructions(instructionIndex)
      val signature = getSignature(state, instruction)
      val input = signature.inputs
      if (input.length > state.length)
        throw new StackDoesNotFitInstructionInput(instruction, input, state)

      val stackTop = state.takeRight(input.length)
      if (input != stackTop)
        throw new StackDoesNotFitInstructionInput(instruction, input, state)

      val remainingStack = state.dropRight(input.length)
      val newStack = remainingStack ++ signature.outputs
      newStack
    }
    catch {
      case e: IndexOutOfBoundsException => throw MissingReturnInstruction
    }
  }

  object MissingReturnInstruction extends BadInputException

}
