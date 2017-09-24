package transformations.bytecode.coreInstructions.integers

import core.particles.CompilationState
import core.particles.node.{Key, Node}
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.{InstructionDelta, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState

object IncrementIntegerDelta extends InstructionDelta {

  override val key: Key = IntegerIncrementKey

  def integerIncrement(location: Int, amount: Int) = CodeAttribute.instruction(IntegerIncrementKey, Seq(location, amount))

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val arguments = CodeAttribute.getInstructionArguments(instruction)
    hexToBytes("84") ++
      byteToBytes(arguments(0)) ++
      byteToBytes(arguments(1))
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: CompilationState): InstructionSignature =
    InstructionSignature(Seq.empty, Seq.empty)

  override def getInstructionSize(): Int = 3

  object IntegerIncrementKey extends Key

  override def description: String = "Defines the increment integer instruction, which increments an integer variable by a specific amount."

  override def grammarName = "iinc"
}
