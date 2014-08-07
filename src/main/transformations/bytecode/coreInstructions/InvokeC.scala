package transformations.bytecode.coreInstructions

import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton
import transformations.javac.base.ConstantPool
import transformations.javac.types.TypeC

abstract class InvokeC extends InstructionC {
  override def getInstructionStackSizeModification(constantPool: ConstantPool, instruction: MetaObject, state: TransformationState): Int = {
    def getMethodStackSizeModification(descriptor: MetaObject): Int = {
      var result = TypeC.getTypeSize(ByteCodeSkeleton.getMethodDescriptorReturnType(descriptor), state)
      for (parameter <- ByteCodeSkeleton.getMethodDescriptorParameters(descriptor))
        result -= TypeC.getTypeSize(parameter, state)
      result
    }

    def getInvokeStackSizeModification(constantPool: ConstantPool, instruction: MetaObject): Int = {
      val location = ByteCodeSkeleton.getInstructionArguments(instruction)(0)
      val methodRef = ByteCodeSkeleton.constantPoolGet(constantPool, location).asInstanceOf[MetaObject]
      val nameAndType = ByteCodeSkeleton.constantPoolGet(constantPool, ByteCodeSkeleton.getMethodRefMethodNameIndex(methodRef)).asInstanceOf[MetaObject]
      val descriptor = ByteCodeSkeleton.constantPoolGet(constantPool, ByteCodeSkeleton.getNameAndTypeType(nameAndType)).asInstanceOf[MetaObject]
      getMethodStackSizeModification(descriptor)
    }
    getInvokeStackSizeModification(constantPool, instruction)
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
    val ins = ByteCodeSkeleton.getMethodDescriptorParameters(descriptor).map(TypeC.toStackType)
    val outs = Seq(TypeC.toStackType(ByteCodeSkeleton.getMethodDescriptorReturnType(descriptor)))
    (ins, outs)
  }

  override def getInstructionSize: Int = 3
}
