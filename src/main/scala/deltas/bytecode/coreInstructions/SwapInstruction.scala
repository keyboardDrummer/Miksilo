package deltas.bytecode.coreInstructions

import core.deltas.Language
import core.deltas.node.{Node, NodeClass}
import deltas.bytecode.PrintByteCode
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.simpleBytecode.ProgramTypeState

object SwapInstruction extends InstructionDelta {

  def swap = CodeAttributeDelta.instruction(key)

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    PrintByteCode.hexToBytes("5f")
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = {
    val stackTop = typeState.stackTypes.takeRight(2)
    new InstructionSignature(stackTop, stackTop.reverse)
  }

  override def description: String = "Defines the swap instruction, which swap the top two values on the stack."

  override def grammarName = "swap"
}
