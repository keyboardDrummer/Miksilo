package transformations.bytecode.coreInstructions

import core.particles.CompilationState
import core.particles.node.{Key, Node}
import transformations.bytecode.PrintByteCode
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.simpleBytecode.ProgramTypeState

object SwapInstruction extends InstructionDelta {
  object SwapKey extends Key
  def swap = CodeAttribute.instruction(SwapKey)

  override val key: Key = SwapKey

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    PrintByteCode.hexToBytes("5f")
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = {
    val stackTop = typeState.stackTypes.takeRight(2)
    new InstructionSignature(stackTop, stackTop.reverse)
  }

  override def description: String = "Defines the swap instruction, which swap the top two values on the stack."
}
