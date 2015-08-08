package transformations.bytecode.coreInstructions.integers

import core.particles.CompilationState
import core.particles.node.{Node, Key}
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.{InstructionSignature, InstructionC}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.types.IntTypeC
import transformations.bytecode.PrintByteCode._

object LoadConstantIntC extends InstructionC
{
  object LoadConstantKey extends Key
  override val key = LoadConstantKey

  def integerConstant(value: Int) = {
    CodeAttribute.instruction(LoadConstantKey, Seq(value))
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = {
    InstructionSignature(Seq.empty, Seq(IntTypeC.intType))
  }

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    val index: Int = CodeAttribute.getInstructionArguments(instruction).head
    hexToBytes("12") ++ byteToBytes(index)
  }
}
