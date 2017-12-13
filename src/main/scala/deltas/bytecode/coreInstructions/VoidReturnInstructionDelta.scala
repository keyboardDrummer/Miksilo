package deltas.bytecode.coreInstructions

import core.deltas.Language
import core.deltas.node.{Node, NodeShape}
import deltas.bytecode.PrintByteCode
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.attributes.CodeAttributeDelta.JumpBehavior
import deltas.bytecode.simpleBytecode.ProgramTypeState

object VoidReturnInstructionDelta extends InstructionDelta {

  def voidReturn: Node = CodeAttributeDelta.instruction(key)

  override def jumpBehavior: JumpBehavior = new JumpBehavior(false, false)

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = PrintByteCode.hexToBytes("b1")

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature =
    InstructionSignature(Seq(), Seq())

  override def getInstructionSize: Int = 1

  override def description: String = "Defines the void return instruction, which returns from the current method."

  override def grammarName = "return"
}
