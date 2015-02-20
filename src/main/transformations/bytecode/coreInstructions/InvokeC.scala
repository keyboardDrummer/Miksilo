package transformations.bytecode.coreInstructions

import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton
import transformations.javac.classes.ConstantPool
import transformations.types.TypeC

abstract class InvokeC extends InstructionC {

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, stackTypes: Seq[MetaObject],
                                          state: TransformationState): InstructionSignature = {
    val methodRef = getInvokeTargetMethodRef(instruction, constantPool)
    val nameAndType = constantPool.getValue(ByteCodeSkeleton.getMethodRefMethodNameIndex(methodRef)).asInstanceOf[MetaObject]
    val descriptor = constantPool.getValue(ByteCodeSkeleton.getNameAndTypeType(nameAndType)).asInstanceOf[MetaObject]
    getMethodStackModification(descriptor, constantPool, state)
  }

  def getMethodStackModification(descriptor: MetaObject, constantPool: ConstantPool, state: TransformationState): InstructionSignature = {
    val ins = ByteCodeSkeleton.getMethodDescriptorParameters(descriptor).map(_type => TypeC.toStackType(constantPool, _type))
    val outs = Seq(TypeC.toStackType(constantPool, ByteCodeSkeleton.getMethodDescriptorReturnType(descriptor)))
    InstructionSignature(ins, outs.filter(p => TypeC.getTypeSize(p, state) > 0))
  }

  def getInvokeTargetMethodRef(instruction: MetaObject, constantPool: ConstantPool) = {
    val location = ByteCodeSkeleton.getInstructionArguments(instruction)(0)
    constantPool.getValue(location).asInstanceOf[MetaObject]
  }

  override def getInstructionSize(instruction: MetaObject): Int = 3
}
