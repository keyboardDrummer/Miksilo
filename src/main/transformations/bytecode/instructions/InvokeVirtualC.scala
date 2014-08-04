package transformations.bytecode.instructions

import core.transformation.MetaObject
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.PrintByteCode._
import transformations.javac.base.ConstantPool
import transformations.javac.base.model.{JavaTypes, QualifiedClassName}

object InvokeVirtualC extends InvokeC {

  override val key: Any = InvokeVirtual

  def invokeVirtual(methodRefIndex: Int) = ByteCodeSkeleton.instruction(InvokeVirtual, Seq(methodRefIndex))

  override def getInstructionStackSizeModification(constantPool: ConstantPool, instruction: MetaObject): Int =
    super.getInstructionStackSizeModification(constantPool, instruction) + 1

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    val arguments = ByteCodeSkeleton.getInstructionArguments(instruction)
    hexToBytes("b6") ++ shortToBytes(arguments(0))
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject) = getInvokeVirtualModification(constantPool, instruction)

  def getInvokeVirtualModification(constantPool: ConstantPool, instruction: MetaObject): (Seq[MetaObject], Seq[MetaObject]) = {
    val methodRef = getInvokeTargetMethodRef(instruction, constantPool)
    val nameAndType = constantPool.getValue(ByteCodeSkeleton.getMethodRefMethodNameIndex(methodRef)).asInstanceOf[MetaObject]
    val classRef = constantPool.getValue(ByteCodeSkeleton.getMethodRefClassRefIndex(methodRef)).asInstanceOf[MetaObject]
    val className = constantPool.getValue(ByteCodeSkeleton.getClassRefName(classRef)).asInstanceOf[QualifiedClassName]
    val classType = JavaTypes.objectType(className)
    val descriptor = constantPool.getValue(ByteCodeSkeleton.getNameAndTypeType(nameAndType)).asInstanceOf[MetaObject]
    val (ins, outs) = getMethodStackModification(descriptor)
    (Seq(classType) ++ ins, outs)
  }

  object InvokeVirtual

}
