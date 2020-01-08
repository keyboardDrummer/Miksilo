package deltas.bytecode.coreInstructions

import core.language.node.Node
import core.language.{Compilation, Language}
import deltas.bytecode.PrintByteCode
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.attributes.CodeAttributeDelta.JumpBehavior
import deltas.bytecode.coreInstructions.integers.integerCompare.JumpInstruction
import deltas.bytecode.simpleBytecode.ProgramTypeState

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
