package deltas.bytecode.coreInstructions

import core.language.node.Node
import core.language.{Compilation, Language}
import deltas.bytecode.PrintByteCode
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.simpleBytecode.ProgramTypeState

object Duplicate2InstructionDelta extends InstructionInstance {

  def duplicate = CodeAttributeDelta.instruction(shape, Seq.empty)

  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = {
    PrintByteCode.hexToBytes("5c")
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = {
    val input: Node = typeState.stackTypes.last
    assertDoubleWord(language, input)
    new InstructionSignature(Seq(input),Seq(input, input))
  }

  override def description: String = "Defines the duplicate2 instruction, which duplicates the top two stack values."

  override def grammarName = "dup2"
}
