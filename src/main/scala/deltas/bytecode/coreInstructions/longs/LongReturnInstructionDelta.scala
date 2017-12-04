package deltas.bytecode.coreInstructions.longs

import core.deltas.node.Node
import core.deltas.{Contract, Language}
import deltas.bytecode.PrintByteCode
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.attributes.CodeAttributeDelta.JumpBehavior
import deltas.bytecode.coreInstructions.{InstructionDelta, InstructionSignature}
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.LongTypeC

object LongReturnInstructionDelta extends InstructionDelta {

  def longReturn: Node = CodeAttributeDelta.instruction(key)

  override def jumpBehavior: JumpBehavior = new JumpBehavior(false, false)

  override def getInstructionSize: Int = 1

  override def getSignature(instruction: Node, typeState: ProgramTypeState, language: Language): InstructionSignature =
    InstructionSignature(Seq(LongTypeC.longType), Seq())

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = PrintByteCode.hexToBytes("ad")

  override def dependencies: Set[Contract] = super.dependencies ++ Set(LongTypeC)

  override def description: String = "Defines the long return instruction, which returns a long from the current method."

  override def grammarName = "lreturn"
}
