package deltas.bytecode.coreInstructions

import core.deltas.Compilation
import core.deltas.node.{Key, Node, NodeClass}
import deltas.bytecode.PrintByteCode
import deltas.bytecode.attributes.CodeAttribute
import deltas.bytecode.simpleBytecode.ProgramTypeState

object Pop2Delta extends InstructionDelta {

  object Pop2Key extends NodeClass
  override val key = Pop2Key

  def pop2 = CodeAttribute.instruction(Pop2Key)

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    PrintByteCode.hexToBytes("58")
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: Compilation): InstructionSignature = {
    val input: Node = typeState.stackTypes.last
    assertDoubleWord(state, input)
    InstructionSignature(Seq(input),Seq())
  }

  override def description: String = "Defines the pop2 instruction, which pops the top two values from the stack."

  override def grammarName = "pop2"
}
