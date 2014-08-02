package transformations.bytecode

import core.transformation.MetaObject
import transformations.javac.base.ConstantPool

case class StackDoesNotFitInstructionInput(instruction: Any, inputTypes: Seq[Any], stack: Seq[Any]) extends RuntimeException {
  override def toString = s"StackDoesNotFitInstructionInput: instruction= $instruction; inputTypes= $inputTypes; stack= $stack"
}

class StackAnalysis(constantPool: ConstantPool, instructions: Seq[MetaObject])
  extends InstructionFlowAnalysis[Seq[Any]](instructions) {

  override def combineState(first: Seq[Any], second: Seq[Any]): Seq[Any] = {
    if (first == second)
      return first

    throw new RuntimeException()
  }

  override def updateState(state: Seq[Any], instructionIndex: Int): Seq[Any] = {
    val instruction = instructions(instructionIndex)
    val inputTypes = Instructions.getInstructionInputTypes(constantPool, instruction)
    if (inputTypes.length > state.length)
      throw new StackDoesNotFitInstructionInput(instruction, inputTypes, state)

    val stackTop = state.takeRight(inputTypes.length)
    if (inputTypes != stackTop)
      throw new StackDoesNotFitInstructionInput(instruction, inputTypes, state)

    val remainingStack = state.dropRight(inputTypes.length)
    val newStack = remainingStack ++ Instructions.getInstructionOutputTypes(constantPool, instruction)
    newStack
  }

}
