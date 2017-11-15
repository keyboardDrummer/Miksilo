package deltas.bytecode.coreInstructions

import core.particles.Compilation
import core.particles.node.{Key, Node, NodeClass}
import deltas.bytecode.PrintByteCode
import deltas.bytecode.attributes.CodeAttribute
import deltas.bytecode.simpleBytecode.ProgramTypeState

object SwapInstruction extends InstructionDelta {
  object SwapKey extends NodeClass
  def swap = CodeAttribute.instruction(SwapKey)

  override val key = SwapKey

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    PrintByteCode.hexToBytes("5f")
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: Compilation): InstructionSignature = {
    val stackTop = typeState.stackTypes.takeRight(2)
    new InstructionSignature(stackTop, stackTop.reverse)
  }

  override def description: String = "Defines the swap instruction, which swap the top two values on the stack."

  override def grammarName = "swap"
}
