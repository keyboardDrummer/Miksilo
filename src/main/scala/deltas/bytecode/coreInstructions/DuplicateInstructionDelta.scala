package deltas.bytecode.coreInstructions

import core.deltas.Compilation
import core.deltas.node.{Node, NodeClass}
import deltas.bytecode.PrintByteCode
import deltas.bytecode.attributes.CodeAttribute
import deltas.bytecode.simpleBytecode.ProgramTypeState


object DuplicateInstructionDelta extends InstructionDelta {

  object DuplicateKey extends NodeClass
  def duplicate = CodeAttribute.instruction(DuplicateKey, Seq.empty)

  override val key = DuplicateKey

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    PrintByteCode.hexToBytes("59")
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: Compilation): InstructionSignature = {
    val input: Node = typeState.stackTypes.last
    assertSingleWord(state, input)
    new InstructionSignature(Seq(input),Seq(input, input))
  }

  override def description: String = "Defines the duplicate instruction, which duplicates the top stack value."

  override def grammarName = "dup"
}
