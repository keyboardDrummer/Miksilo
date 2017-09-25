package transformations.bytecode.coreInstructions.integers

import core.particles.node.{Key, Node}
import core.particles.{CompilationState, Contract}
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.{InstructionDelta, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.types.IntTypeC

object AddIntegersDelta extends InstructionDelta {
  override val key: Key = AddIntegersKey

  def addIntegers() = CodeAttribute.instruction(AddIntegersKey)

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = hexToBytes("60")

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = binary(IntTypeC.intType)

  override def getInstructionSize: Int = 1

  object AddIntegersKey extends Key

  override def dependencies: Set[Contract] = super.dependencies ++ Set(IntTypeC)

  override def description: String = "Defines the add integers instruction, which adds the top two stack values together and places the result on the stack."

  override def grammarName = "iadd"
}
