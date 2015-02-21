package transformations.bytecode.simpleBytecode

import core.exceptions.BadInputException
import core.transformation.MetaObject
import transformations.bytecode.ByteCodeSkeleton.JumpBehavior
import transformations.bytecode.coreInstructions.InstructionSignature

case class ProgramTypeState(stackTypes: Seq[MetaObject], variableTypes: Map[Int, MetaObject])

class StackLayoutAnalysis(instructions: Seq[MetaObject],
                          getVariableUpdates: MetaObject => Map[Int, MetaObject],
                          getSignature: (ProgramTypeState, MetaObject) => InstructionSignature,
                          getJumpBehavior: Any => JumpBehavior)
  extends InstructionFlowAnalysis[ProgramTypeState](instructions, getJumpBehavior) {

  case class StackDoesNotFitInstructionInput(instruction: Any, inputTypes: Seq[Any], stack: Seq[Any]) extends RuntimeException {
    override def toString = s"StackDoesNotFitInstructionInput: instruction= $instruction; inputTypes= $inputTypes; stack= $stack"
  }

  case class TargetInstructionEnteredWithDifferentLayouts(first: ProgramTypeState, second: ProgramTypeState) extends RuntimeException
  {
    override def toString = s"TargetInstructionEnteredWithDifferentLayouts, first:=$first, second:= $second"
  }

  override def combineState(first: ProgramTypeState, second: ProgramTypeState): Option[ProgramTypeState] = {
    if (first == second)
      return None

    throw new TargetInstructionEnteredWithDifferentLayouts(first, second)
  }

  override def updateState(state: ProgramTypeState, instructionIndex: Int): ProgramTypeState = {
    try
    {
      val stateStack = state.stackTypes
      val instruction = instructions(instructionIndex)
      val signature = getSignature(state, instruction)
      val input = signature.inputs
      if (input.length > stateStack.length)
        throw new StackDoesNotFitInstructionInput(instruction, input, stateStack)

//      val stackTop = stateStack.takeRight(input.length) //TODO deze typecheck weer terughalen maar met slechts 1 object type.
//      if (input != stackTop)
//        throw new StackDoesNotFitInstructionInput(instruction, input, stateStack)

      val remainingStack = stateStack.dropRight(input.length)
      val newStack = remainingStack ++ signature.outputs

      val updates = getVariableUpdates(instruction)
      val newVariables = updates ++ state.variableTypes
      ProgramTypeState(newStack, newVariables)
    }
    catch {
      case e: IndexOutOfBoundsException => throw MissingReturnInstruction
    }
  }

  object MissingReturnInstruction extends BadInputException

}
