package deltas.bytecode.coreInstructions

import core.deltas.Language
import core.deltas.node.{Node, NodeClass}
import deltas.bytecode.PrintByteCode
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.attributes.CodeAttributeDelta.JumpBehavior
import deltas.bytecode.coreInstructions.integers.integerCompare.JumpInstruction
import deltas.bytecode.simpleBytecode.ProgramTypeState

object GotoDelta extends JumpInstruction {

  override val key = GoToKey

  def goTo(target: Int): Node = CodeAttributeDelta.instruction(GoToKey, Seq(target))

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val arguments = CodeAttributeDelta.getInstructionArguments(instruction)
    PrintByteCode.hexToBytes("a7") ++ PrintByteCode.shortToBytes(arguments(0))
  }

  override def jumpBehavior: JumpBehavior = JumpBehavior(false, true)

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature = InstructionSignature(Seq(), Seq())

  object GoToKey extends NodeClass

  override def description: String = "Defines the goto instruction, which jumps to a target instruction."

  override def grammarName = "goto"
}
