package transformations.bytecode.coreInstructions

import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton
import transformations.javac.classes.ConstantPool
import transformations.types.TypeC

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

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, state: TransformationState) = {
    val methodRef = getInvokeTargetMethodRef(instruction, constantPool)
    val nameAndType = constantPool.getValue(ByteCodeSkeleton.getMethodRefMethodNameIndex(methodRef)).asInstanceOf[MetaObject]
    val descriptor = constantPool.getValue(ByteCodeSkeleton.getNameAndTypeType(nameAndType)).asInstanceOf[MetaObject]
    getMethodStackModification(descriptor, constantPool, state)
  }

  def getMethodStackModification(descriptor: MetaObject, constantPool: ConstantPool, state: TransformationState) = {
    val ins = ByteCodeSkeleton.getMethodDescriptorParameters(descriptor).map(_type => TypeC.toStackType(constantPool, _type))
    val outs = Seq(TypeC.toStackType(constantPool, ByteCodeSkeleton.getMethodDescriptorReturnType(descriptor)))
    (ins, outs.filter(p => TypeC.getTypeSize(p, state) > 0))
  }

  def getInvokeTargetMethodRef(instruction: MetaObject, constantPool: ConstantPool) = {
    val location = ByteCodeSkeleton.getInstructionArguments(instruction)(0)
    constantPool.getValue(location).asInstanceOf[MetaObject]
  }

  override def getInstructionSize(instruction: MetaObject): Int = 3
}
