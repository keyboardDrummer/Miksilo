package miksilo.modularLanguages.deltas.bytecode.coreInstructions

import miksilo.modularLanguages.core.node.{Node, NodeShape}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.bytecode.PrintByteCode
import miksilo.modularLanguages.deltas.bytecode.attributes.CodeAttributeDelta
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.ProgramTypeState

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
