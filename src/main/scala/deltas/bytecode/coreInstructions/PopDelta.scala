package deltas.bytecode.coreInstructions

import core.deltas.Compilation
import core.deltas.node.{Key, Node, NodeClass}
import deltas.bytecode.PrintByteCode
import deltas.bytecode.attributes.CodeAttribute
import deltas.bytecode.simpleBytecode.ProgramTypeState

object PopDelta extends InstructionDelta {

  object PopKey extends NodeClass
  override val key = PopKey

  def pop = CodeAttribute.instruction(PopKey)

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    PrintByteCode.hexToBytes("57")
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: Compilation): InstructionSignature = {
    val input: Node = typeState.stackTypes.last
    assertSingleWord(state, input)
    InstructionSignature(Seq(input),Seq())
  }

  override def description: String = "Defines the pop instruction, which pops the top value from the stack."

  override def grammarName = "pop"
}
