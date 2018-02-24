package deltas.bytecode.coreInstructions

import core.language.node.{Node, NodeShape}
import core.language.{Compilation, Language}
import deltas.bytecode.PrintByteCode
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.simpleBytecode.ProgramTypeState

object Pop2Delta extends InstructionInstance {

  def pop2 = CodeAttributeDelta.instruction(shape)

  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = {
    PrintByteCode.hexToBytes("58")
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = {
    val input: Node = typeState.stackTypes.last
    assertDoubleWord(language, input)
    InstructionSignature(Seq(input),Seq())
  }

  override def description: String = "Defines the pop2 instruction, which pops the top two values from the stack."

  override def grammarName = "pop2"
}
