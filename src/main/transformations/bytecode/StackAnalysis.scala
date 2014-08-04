package transformations.bytecode

import core.transformation.MetaObject

case class StackDoesNotFitInstructionInput(instruction: Any, inputTypes: Seq[Any], stack: Seq[Any]) extends RuntimeException {
  override def toString = s"StackDoesNotFitInstructionInput: instruction= $instruction; inputTypes= $inputTypes; stack= $stack"
}

class StackAnalysis(instructions: Seq[MetaObject], getInputTypes: MetaObject => Seq[MetaObject], getOutputTypes: MetaObject => Seq[MetaObject])
  extends InstructionFlowAnalysis[Seq[MetaObject]](instructions) {

  override def combineState(first: Seq[MetaObject], second: Seq[MetaObject]): Seq[MetaObject] = {
    if (first == second)
      return first

    throw new RuntimeException()
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
