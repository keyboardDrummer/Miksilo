package transformations.bytecode.simpleBytecode

import core.transformation.{MetaObject, TransformationState}


class StackLayoutAnalysis(instructions: Seq[MetaObject], getInputTypes: MetaObject => Seq[MetaObject], getOutputTypes: MetaObject => Seq[MetaObject],
                          state: TransformationState)
  extends InstructionFlowAnalysis[Seq[MetaObject]](instructions, state) {

  case class StackDoesNotFitInstructionInput(instruction: Any, inputTypes: Seq[Any], stack: Seq[Any]) extends RuntimeException {
    override def toString = s"StackDoesNotFitInstructionInput: instruction= $instruction; inputTypes= $inputTypes; stack= $stack"
  }

  case class TargetInstructionEnteredWithDifferentLayouts(first: Seq[MetaObject], second: Seq[MetaObject]) extends RuntimeException

  override def combineState(first: Seq[MetaObject], second: Seq[MetaObject]): Option[Seq[MetaObject]] = {
    if (first == second)
      return None

    throw new TargetInstructionEnteredWithDifferentLayouts(first, second)
  }

  override def updateState(state: Seq[MetaObject], instructionIndex: Int): Seq[MetaObject] = {
    val instruction = instructions(instructionIndex)
    val inputTypes = getInputTypes(instruction)
    if (inputTypes.length > state.length)
      throw new StackDoesNotFitInstructionInput(instruction, inputTypes, state)

    val stackTop = state.takeRight(inputTypes.length)
    if (inputTypes != stackTop)
      throw new StackDoesNotFitInstructionInput(instruction, inputTypes, state)

    val remainingStack = state.dropRight(inputTypes.length)
    val newStack = remainingStack ++ getOutputTypes(instruction)
    newStack
  }

}
