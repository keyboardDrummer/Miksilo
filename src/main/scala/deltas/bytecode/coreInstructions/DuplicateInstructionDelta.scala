package deltas.bytecode.coreInstructions

import core.deltas.Language
import core.deltas.node.{Node, NodeClass}
import deltas.bytecode.PrintByteCode
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.simpleBytecode.ProgramTypeState


object DuplicateInstructionDelta extends InstructionDelta {

  object DuplicateKey extends NodeClass
  def duplicate = CodeAttributeDelta.instruction(DuplicateKey, Seq.empty)

  override val key = DuplicateKey

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    PrintByteCode.hexToBytes("59")
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = {
    val input: Node = typeState.stackTypes.last
    assertSingleWord(language, input)
    new InstructionSignature(Seq(input),Seq(input, input))
  }

  override def description: String = "Defines the duplicate instruction, which duplicates the top stack value."

  override def grammarName = "dup"
}
