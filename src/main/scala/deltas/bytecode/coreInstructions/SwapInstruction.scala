package deltas.bytecode.coreInstructions

import core.language.node.{Node, NodeShape}
import core.language.{Compilation, Language}
import deltas.bytecode.PrintByteCode
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.simpleBytecode.ProgramTypeState

object SwapInstruction extends InstructionInstance {

  def swap = CodeAttributeDelta.instruction(shape)

  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = {
    PrintByteCode.hexToBytes("5f")
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = {
    val stackTop = typeState.stackTypes.takeRight(2)
    new InstructionSignature(stackTop, stackTop.reverse)
  }

  override def description: String = "Defines the swap instruction, which swap the top two values on the stack."

  override def grammarName = "swap"
}
