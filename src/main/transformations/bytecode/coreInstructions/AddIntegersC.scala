package transformations.bytecode.coreInstructions

import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.PrintByteCode._
import transformations.javac.classes.ConstantPool
import transformations.javac.types.IntTypeC

object AddIntegersC extends InstructionC {
  override val key: AnyRef = AddIntegersKey

  def addInteger = instruction(AddIntegersKey)

  override def getInstructionStackSizeModification(constantPool: ConstantPool, instruction: MetaObject, state: TransformationState): Int = -1

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = hexToBytes("60")

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject) = binary(IntTypeC.intType)

  override def getInstructionSize(instruction: MetaObject): Int = 1

  object AddIntegersKey

}
