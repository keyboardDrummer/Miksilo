package deltas.bytecode.coreInstructions.longs

import core.particles.node.{Key, Node, NodeClass}
import core.particles.{Compilation, Contract}
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.attributes.CodeAttribute
import deltas.bytecode.coreInstructions.{InstructionDelta, InstructionSignature}
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.LongTypeC

object AddLongsDelta extends InstructionDelta {
  override val key = AddLongsKey

  def addLongs() = CodeAttribute.instruction(AddLongsKey)

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = hexToBytes("61")

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: Compilation): InstructionSignature = binary(LongTypeC.longType)

  override def getInstructionSize: Int = 1

  object AddLongsKey extends NodeClass

  override def dependencies: Set[Contract] = super.dependencies ++ Set(LongTypeC)

  override def description: String = "Defines the add longs instruction, which adds the top two stack values together and places the result on the stack."

  override def grammarName = "ladd"
}
