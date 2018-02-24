package deltas.bytecode.coreInstructions

import core.language.node.{Node, NodeShape}
import core.language.{Compilation, Language}
import deltas.bytecode.PrintByteCode
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.simpleBytecode.ProgramTypeState

object PopDelta extends InstructionInstance {

  def pop = CodeAttributeDelta.instruction(shape)

  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = {
    PrintByteCode.hexToBytes("57")
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = {
    val input: Node = typeState.stackTypes.last
    assertSingleWord(language, input)
    InstructionSignature(Seq(input),Seq())
  }

  override def description: String = "Defines the pop instruction, which pops the top value from the stack."

  override def grammarName = "pop"
}
