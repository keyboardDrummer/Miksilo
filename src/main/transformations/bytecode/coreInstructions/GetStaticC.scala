package transformations.bytecode.coreInstructions

import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.PrintByteCode._
import transformations.javac.base.ConstantPool

object GetStaticC extends InstructionC {

  override val key: AnyRef = GetStaticKey

  def getStatic(fieldRefIndex: Int): MetaObject = instruction(GetStaticKey, Seq(fieldRefIndex))

  override def getInstructionStackSizeModification(constantPool: ConstantPool, instruction: MetaObject, state: TransformationState): Int = 1

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    val arguments = ByteCodeSkeleton.getInstructionArguments(instruction)
    hexToBytes("b2") ++ shortToBytes(arguments(0))
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject) = (Seq(), Seq(getReturnType(constantPool, instruction)))

  def getReturnType(constantPool: ConstantPool, getStatic: MetaObject): MetaObject = {
    val location = ByteCodeSkeleton.getInstructionArguments(getStatic)(0)
    val fieldRef = constantPool.getValue(location).asInstanceOf[MetaObject]
    val nameAndType = constantPool.getValue(ByteCodeSkeleton.getFieldRefNameAndTypeIndex(fieldRef)).asInstanceOf[MetaObject]
    val fieldType = constantPool.getValue(ByteCodeSkeleton.getNameAndTypeType(nameAndType)).asInstanceOf[MetaObject]
    fieldType
  }

  override def getInstructionSize: Int = 3

  object GetStaticKey

}
