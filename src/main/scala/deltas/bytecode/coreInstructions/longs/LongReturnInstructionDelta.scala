package deltas.bytecode.coreInstructions.longs

import core.deltas.node.{Node, NodeClass}
import core.deltas.{Compilation, Contract}
import deltas.bytecode.PrintByteCode
import deltas.bytecode.attributes.CodeAttribute
import deltas.bytecode.attributes.CodeAttribute.JumpBehavior
import deltas.bytecode.coreInstructions.{InstructionDelta, InstructionSignature}
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.LongTypeC

object LongReturnInstructionDelta extends InstructionDelta {

  override val key = LongReturn

  def longReturn: Node = CodeAttribute.instruction(LongReturn)

  override def jumpBehavior: JumpBehavior = new JumpBehavior(false, false)

  override def getInstructionSize: Int = 1

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: Compilation): InstructionSignature =
    InstructionSignature(Seq(LongTypeC.longType), Seq())

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = PrintByteCode.hexToBytes("ad")

  object LongReturn extends NodeClass

  override def dependencies: Set[Contract] = super.dependencies ++ Set(LongTypeC)

  override def description: String = "Defines the long return instruction, which returns a long from the current method."

  override def grammarName = "lreturn"
}
