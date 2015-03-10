package transformations.bytecode.coreInstructions.longs

import core.particles.{CompilationState, MetaObject}
import transformations.bytecode.PrintByteCode._
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.{InstructionC, InstructionSignature}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.javac.classes.ConstantPool
import transformations.types.LongTypeC

object LongConstantC extends InstructionC {

  override val key: AnyRef = LongConstantKey

  def constant(value: Int) = {
    require (0 <= value && value <= 1)
    CodeAttribute.instruction(LongConstantKey, Seq(value))
  }

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    byteToBytes(9 + CodeAttribute.getInstructionArguments(instruction)(0))
  }

  override def getSignature(instruction: MetaObject, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = InstructionSignature(Seq(), Seq(LongTypeC.longType))

  override def getInstructionSize(): Int = 1

  private object LongConstantKey
}