package transformations.bytecode.coreInstructions

import core.particles.{CompilationState, MetaObject}
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.constants.{ClassRefConstant, MethodDescriptorConstant, MethodRefConstant, NameAndType}
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.javac.classes.{ConstantPool, QualifiedClassName}
import transformations.types.{ObjectTypeC, TypeSkeleton}

abstract class InvokeC extends InstructionC {

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = {
    val methodRef = getInvokeTargetMethodRef(instruction, constantPool)
    val nameAndType = constantPool.getValue(MethodRefConstant.getMethodRefMethodNameIndex(methodRef)).asInstanceOf[MetaObject]
    val descriptor = constantPool.getValue(NameAndType.getTypeIndex(nameAndType)).asInstanceOf[MetaObject]
    getMethodStackModification(descriptor, constantPool, state)
  }

  def getInstanceInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, typeState: ProgramTypeState,
                                          state: CompilationState): InstructionSignature = {
    val methodRef = getInvokeTargetMethodRef(instruction, constantPool)
    val nameAndType = constantPool.getValue(MethodRefConstant.getMethodRefMethodNameIndex(methodRef)).asInstanceOf[MetaObject]
    val classRef = constantPool.getValue(MethodRefConstant.getMethodRefClassRefIndex(methodRef)).asInstanceOf[MetaObject]
    val className = constantPool.getValue(ClassRefConstant.getNameIndex(classRef)).asInstanceOf[QualifiedClassName]
    val classType = ObjectTypeC.objectType(className)
    val descriptor = constantPool.getValue(NameAndType.getTypeIndex(nameAndType)).asInstanceOf[MetaObject]
    val InstructionSignature(ins, outs) = getMethodStackModification(descriptor, constantPool, state)
    InstructionSignature(Seq(classType) ++ ins, outs)
  }

  def getMethodStackModification(descriptor: MetaObject, constantPool: ConstantPool, state: CompilationState): InstructionSignature = {
    val ins = MethodDescriptorConstant.getMethodDescriptorParameters(descriptor).map(_type => TypeSkeleton.toStackType(_type, state))
    val outs = Seq(TypeSkeleton.toStackType(MethodDescriptorConstant.getMethodDescriptorReturnType(descriptor), state))
    InstructionSignature(ins, outs.filter(p => TypeSkeleton.getTypeSize(p, state) > 0))
  }

  def getInvokeTargetMethodRef(instruction: MetaObject, constantPool: ConstantPool) = {
    val location = CodeAttribute.getInstructionArguments(instruction)(0)
    constantPool.getValue(location).asInstanceOf[MetaObject]
  }

  override def getInstructionSize: Int = 3
}
