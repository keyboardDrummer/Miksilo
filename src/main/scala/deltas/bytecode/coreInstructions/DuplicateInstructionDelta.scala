package deltas.bytecode.coreInstructions

import core.language.node.Node
import core.language.{Compilation, Language}
import deltas.bytecode.PrintByteCode
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.simpleBytecode.ProgramTypeState


object DuplicateInstructionDelta extends InstructionInstance {

  def duplicate = CodeAttributeDelta.instruction(shape, Seq.empty)

  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = {
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
