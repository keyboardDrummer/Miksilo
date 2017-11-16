package deltas.bytecode.coreInstructions.integers

import core.deltas.node.{Key, Node, NodeClass}
import core.deltas.{Compilation, Contract}
import deltas.bytecode.PrintByteCode._
import deltas.bytecode.attributes.CodeAttribute
import deltas.bytecode.coreInstructions.{InstructionDelta, InstructionSignature}
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.IntTypeC

object AddIntegersDelta extends InstructionDelta {
  override val key = AddIntegersKey

  def addIntegers() = CodeAttribute.instruction(AddIntegersKey)

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = hexToBytes("60")

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: Compilation): InstructionSignature = binary(IntTypeC.intType)

  override def getInstructionSize: Int = 1

  object AddIntegersKey extends NodeClass

  override def dependencies: Set[Contract] = super.dependencies ++ Set(IntTypeC)

  override def description: String = "Defines the add integers instruction, which adds the top two stack values together and places the result on the stack."

  override def grammarName = "iadd"
}
