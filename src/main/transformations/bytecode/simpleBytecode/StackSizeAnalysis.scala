package transformations.bytecode.simpleBytecode

import core.transformation.{MetaObject, TransformationState}

class StackSizeAnalysis(instructions: Seq[MetaObject], getInstructionStackSizeModification: MetaObject => Int, state: TransformationState)
  extends InstructionFlowAnalysis[Int](instructions, state) {

  case class DifferentSizedStacks(first: Int, second: Int) extends RuntimeException

  override def combineState(first: Int, second: Int): Option[Int] = {
    if (first == second)
      return None

    throw new DifferentSizedStacks(first, second)
  }

  override def updateState(state: Int, instructionIndex: Int): Int = {
    val instruction = instructions(instructionIndex)
    val result = state + getInstructionStackSizeModification(instruction)
    if (result < 0)
      throw new RuntimeException()
    result
  }
}
