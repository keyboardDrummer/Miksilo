package deltas.bytecode.coreInstructions.objects

import core.deltas.node.{Node, NodeShape}
import core.deltas.{Contract, Language}
import deltas.bytecode.PrintByteCode
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.attributes.CodeAttributeDelta.JumpBehavior
import deltas.bytecode.coreInstructions.{InstructionDelta, InstructionSignature}
import deltas.bytecode.simpleBytecode.ProgramTypeState

object AddressReturnInstructionDelta extends InstructionDelta {

  def create: Node = CodeAttributeDelta.instruction(key)

  override def jumpBehavior: JumpBehavior = new JumpBehavior(false, false)

  override def getInstructionSize: Int = 1

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature =
    InstructionSignature(Seq(typeState.stackTypes.head), Seq())

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = PrintByteCode.hexToBytes("b0")

  override def dependencies: Set[Contract] = super.dependencies ++ Set()

  override def description: String = "Defines the address return instruction, which returns a reference from the current method."

  override def grammarName = "areturn"
}
