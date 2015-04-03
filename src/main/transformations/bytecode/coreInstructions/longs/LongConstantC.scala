package transformations.bytecode.coreInstructions.longs

import core.particles.CompilationState
import core.particles.node.Node
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.{InstructionC, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.javac.classes.ConstantPool
import transformations.bytecode.types.LongTypeC

object LongConstantC extends InstructionC {

  override val key: AnyRef = LongConstantKey

  def constant(value: Int) = {
    require (0 <= value && value <= 1)
    CodeAttribute.instruction(LongConstantKey, Seq(value))
  }

  override def getInstructionByteCode(instruction: Node): Seq[Byte] = {
    byteToBytes(9 + CodeAttribute.getInstructionArguments(instruction)(0))
  }

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = InstructionSignature(Seq(), Seq(LongTypeC.longType))

  override def getInstructionSize(): Int = 1

  private object LongConstantKey
}