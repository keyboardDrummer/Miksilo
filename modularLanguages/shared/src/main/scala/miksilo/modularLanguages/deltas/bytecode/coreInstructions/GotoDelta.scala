package miksilo.modularLanguages.deltas.bytecode.coreInstructions

import miksilo.modularLanguages.core.node.Node
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.deltas.bytecode.PrintByteCode
import miksilo.modularLanguages.deltas.bytecode.attributes.CodeAttributeDelta
import miksilo.modularLanguages.deltas.bytecode.attributes.CodeAttributeDelta.JumpBehavior
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.integers.integerCompare.JumpInstruction
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.ProgramTypeState

object GotoDelta extends JumpInstruction {

  def goTo(target: Int): Node = CodeAttributeDelta.instruction(shape, Seq(target))

  override def getBytes(compilation: Compilation, instruction: Node): Seq[Byte] = {
    val arguments = CodeAttributeDelta.getInstructionArguments(instruction)
    PrintByteCode.hexToBytes("a7") ++ PrintByteCode.shortToBytes(arguments(0))
  }

  override def jumpBehavior: JumpBehavior = JumpBehavior(false, true)

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = InstructionSignature(Seq(), Seq())

  override def description: String = "Defines the goto instruction, which jumps to a target instruction."

  override def grammarName = "goto"
}
