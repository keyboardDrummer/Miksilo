package transformations.bytecode.coreInstructions.longs

import core.particles.{MetaObject, CompilationState}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.{PrintByteCode, ByteCodeSkeleton}
import transformations.bytecode.ByteCodeSkeleton._
import PrintByteCode._
import transformations.bytecode.coreInstructions.{InstructionSignature, InstructionC}
import transformations.javac.classes.ConstantPool
import transformations.types.LongTypeC

object LongConstantC extends InstructionC {

  override val key: AnyRef = LongConstantKey

  def constant(value: Int) = {
    require (0 <= value && value <= 1)
    instruction(LongConstantKey, Seq(value))
  }

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    byteToBytes(9 + ByteCodeSkeleton.getInstructionArguments(instruction)(0))
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = InstructionSignature(Seq(), Seq(LongTypeC.longType))

  override def getInstructionSize(instruction: MetaObject): Int = 1

  private object LongConstantKey
}