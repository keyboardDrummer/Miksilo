package transformations.bytecode.coreInstructions.integers

import core.particles.CompilationState
import core.particles.node.{Key, Node}
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.{InstructionDelta, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.types.IntTypeC

object SmallIntegerConstantDelta extends InstructionDelta {

  override val key: Key = IntegerConstantKey

  def integerConstant(value: Int) = {
    require (value <= 5)
    require (value >= -1)
    CodeAttribute.instruction(IntegerConstantKey, Seq(value))
  }

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    byteToBytes(3 + CodeAttribute.getInstructionArguments(instruction).head)
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: CompilationState): InstructionSignature =
    InstructionSignature(Seq(), Seq(IntTypeC.intType))

  override def getInstructionSize: Int = 1

  private object IntegerConstantKey extends Key

  override def description: String = "Defines the integer constant instruction, which places an integer between -1 and 5 on the stack."

  override def grammarName = "iconst" //TODO eigenlijk heb je ook nog iconst_0 etc.. maar die zitten verbogen in deze Delta.
}
