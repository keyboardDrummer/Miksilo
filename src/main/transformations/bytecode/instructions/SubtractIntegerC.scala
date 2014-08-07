package transformations.bytecode.instructions

import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.PrintByteCode._
import transformations.javac.base.ConstantPool
import transformations.javac.types.IntTypeC

object SubtractIntegerC extends InstructionC {
  override val key: AnyRef = SubtractIntegerKey

  def subtractInteger = instruction(SubtractIntegerKey)

  override def getInstructionStackSizeModification(constantPool: ConstantPool, instruction: MetaObject, state: TransformationState): Int = -1

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = hexToBytes("64")

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject) = binary(IntTypeC.intType)

  override def getInstructionSize: Int = 1

  object SubtractIntegerKey

}
