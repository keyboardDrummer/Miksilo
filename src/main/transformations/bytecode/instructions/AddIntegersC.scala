package transformations.bytecode.instructions

import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.PrintByteCode._
import transformations.javac.base.ConstantPool
import transformations.javac.types.IntTypeC

object AddIntegersC extends InstructionC {
  override val key: Any = AddIntegersKey

  def addInteger = instruction(AddIntegersKey)

  override def getInstructionStackSizeModification(constantPool: ConstantPool, instruction: MetaObject, state: TransformationState): Int = -1

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = hexToBytes("60")

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject) = binary(IntTypeC.intType)

  override def getInstructionSize: Int = 1

  object AddIntegersKey

}
