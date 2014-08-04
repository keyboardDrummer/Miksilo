package transformations.bytecode.instructions

import core.transformation.MetaObject
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.PrintByteCode._
import transformations.javac.base.ConstantPool
import transformations.javac.base.model.JavaTypes

object AddIntegersC extends InstructionC {
  override val key: Any = AddIntegersKey

  def addInteger = instruction(AddIntegersKey)

  override def getInstructionStackSizeModification(constantPool: ConstantPool, instruction: MetaObject): Int = -1

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = hexToBytes("60")

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject): (Seq[MetaObject], Seq[MetaObject]) = binary(JavaTypes.intType)

  override def getInstructionSize: Int = 1

  object AddIntegersKey

}
