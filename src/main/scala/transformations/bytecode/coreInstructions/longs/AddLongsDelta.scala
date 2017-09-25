package transformations.bytecode.coreInstructions.longs

import core.particles.node.{Key, Node}
import core.particles.{CompilationState, Contract}
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.{InstructionDelta, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.types.LongTypeC

object AddLongsDelta extends InstructionDelta {
  override val key: Key = AddLongsKey

  def addLongs() = CodeAttribute.instruction(AddLongsKey)

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = hexToBytes("61")

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = binary(LongTypeC.longType)

  override def getInstructionSize: Int = 1

  object AddLongsKey extends Key

  override def dependencies: Set[Contract] = super.dependencies ++ Set(LongTypeC)

  override def description: String = "Defines the add longs instruction, which adds the top two stack values together and places the result on the stack."

  override def grammarName = "ladd"
}
