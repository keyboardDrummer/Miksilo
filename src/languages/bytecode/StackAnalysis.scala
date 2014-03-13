package languages.bytecode

import transformation.MetaObject
import languages.javac.base.ConstantPool

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
      throw new RuntimeException()

    val stackTop = state.takeRight(inputTypes.length)
    if (inputTypes != stackTop)
      throw new RuntimeException

    val remainingStack = state.dropRight(inputTypes.length)
    val newStack = remainingStack ++ Instructions.getInstructionOutputTypes(constantPool, instruction)
    newStack
  }

}
