package deltas.bytecode.coreInstructions.integers

import core.deltas.node.{Key, Node, NodeClass}
import core.deltas.{Compilation, Contract}
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.attributes.CodeAttribute
import deltas.bytecode.coreInstructions.{InstructionDelta, InstructionSignature}
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.IntTypeC

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
