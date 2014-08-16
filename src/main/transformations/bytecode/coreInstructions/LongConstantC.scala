package transformations.bytecode.coreInstructions

import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.PrintByteCode._
import transformations.javac.classes.ConstantPool
import transformations.types.LongTypeC

object LongConstantC extends InstructionC {

  override val key: AnyRef = LongConstantKey

  def constant(value: Int) = {
    require (value <= 1)
    require (value >= 0)
    instruction(LongConstantKey, Seq(value))
  }

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    byteToBytes(9 + ByteCodeSkeleton.getInstructionArguments(instruction)(0))
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, state: TransformationState)
  = (Seq(), Seq(LongTypeC.longType))

  override def getInstructionSize(instruction: MetaObject): Int = 1

  private object LongConstantKey
}