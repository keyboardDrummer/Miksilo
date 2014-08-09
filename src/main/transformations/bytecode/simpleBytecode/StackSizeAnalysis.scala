package transformations.bytecode.simpleBytecode

import core.transformation.{MetaObject, TransformationState}

class StackSizeAnalysis(instructions: Seq[MetaObject], getInstructionStackSizeModification: MetaObject => Int, state: TransformationState)
  extends InstructionFlowAnalysis[Int](instructions, state) {

  override def combineState(first: Int, second: Int): Option[Int] = {
    if (first == second)
      return None

    throw new RuntimeException()
  }

  override def updateState(state: Int, instructionIndex: Int): Int = {
    val instruction = instructions(instructionIndex)
    val result = state + getInstructionStackSizeModification(instruction)
    if (result < 0)
      throw new RuntimeException()
    result
  }
}
