package transformations.bytecode.instructions

import core.transformation.MetaObject
import transformations.bytecode.ByteCodeSkeleton
import transformations.javac.base.ConstantPool
import transformations.javac.base.model.JavaTypes

abstract class InvokeC extends InstructionC {
  override def getInstructionStackSizeModification(constantPool: ConstantPool, instruction: MetaObject): Int = getInvokeStackSizeModification(constantPool, instruction)

  def getInvokeStackSizeModification(constantPool: ConstantPool, instruction: MetaObject): Int = {
    val location = ByteCodeSkeleton.getInstructionArguments(instruction)(0)
    val methodRef = ByteCodeSkeleton.constantPoolGet(constantPool, location).asInstanceOf[MetaObject]
    val nameAndType = ByteCodeSkeleton.constantPoolGet(constantPool, ByteCodeSkeleton.getMethodRefMethodNameIndex(methodRef)).asInstanceOf[MetaObject]
    val descriptor = ByteCodeSkeleton.constantPoolGet(constantPool, ByteCodeSkeleton.getNameAndTypeType(nameAndType)).asInstanceOf[MetaObject]
    getMethodStackSizeModification(descriptor)
  }

  def getMethodStackSizeModification(descriptor: MetaObject): Int = {
    var result = ByteCodeSkeleton.getTypeSize(ByteCodeSkeleton.getMethodDescriptorReturnType(descriptor))
    for (parameter <- ByteCodeSkeleton.getMethodDescriptorParameters(descriptor))
      result -= ByteCodeSkeleton.getTypeSize(parameter)
    result
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject) =
    getInvokeStackModification(constantPool, instruction)

  def getInvokeStackModification(constantPool: ConstantPool, instruction: MetaObject): (Seq[MetaObject], Seq[MetaObject]) = {
    val methodRef = getInvokeTargetMethodRef(instruction, constantPool)
    val nameAndType = constantPool.getValue(ByteCodeSkeleton.getMethodRefMethodNameIndex(methodRef)).asInstanceOf[MetaObject]
    val descriptor = constantPool.getValue(ByteCodeSkeleton.getNameAndTypeType(nameAndType)).asInstanceOf[MetaObject]
    getMethodStackModification(descriptor)
  }

  def getInvokeTargetMethodRef(instruction: MetaObject, constantPool: ConstantPool) = {
    val location = ByteCodeSkeleton.getInstructionArguments(instruction)(0)
    constantPool.getValue(location).asInstanceOf[MetaObject]
  }

  def getMethodStackModification(descriptor: MetaObject): (Seq[MetaObject], Seq[MetaObject]) = {
    val ins = ByteCodeSkeleton.getMethodDescriptorParameters(descriptor).map(JavaTypes.javaTypeToByteCodeType)
    val outs = Seq(JavaTypes.javaTypeToByteCodeType(ByteCodeSkeleton.getMethodDescriptorReturnType(descriptor)))
    (ins, outs)
  }

  override def getInstructionSize: Int = 3
}
