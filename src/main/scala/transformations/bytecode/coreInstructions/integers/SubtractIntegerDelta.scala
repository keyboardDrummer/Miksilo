package transformations.bytecode.coreInstructions.integers

import core.particles.node.{Key, Node, NodeClass}
import core.particles.{Compilation, Contract}
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.{InstructionDelta, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.types.IntTypeC

object SubtractIntegerDelta extends InstructionDelta {
  override val key = SubtractIntegerKey

  def subtractInteger = CodeAttribute.instruction(SubtractIntegerKey)

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = hexToBytes("64")

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: Compilation): InstructionSignature = binary(IntTypeC.intType)

  override def getInstructionSize: Int = 1

  object SubtractIntegerKey extends NodeClass
  override def dependencies: Set[Contract] = super.dependencies ++ Set(IntTypeC)

  override def description: String = "Defines the subtract integer instruction, which subtracts the top two integer on the stack and places the result on the stack."

  override def grammarName = "isub"
}
