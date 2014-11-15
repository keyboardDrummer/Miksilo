package transformations.bytecode.coreInstructions

import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.{PrintByteCode, ByteCodeSkeleton}
import PrintByteCode._
import transformations.javac.classes.{ConstantPool, QualifiedClassName}
import transformations.types.ObjectTypeC

object InvokeVirtualC extends InvokeC {

  override val key: AnyRef = InvokeVirtual

  def invokeVirtual(methodRefIndex: Int) = ByteCodeSkeleton.instruction(InvokeVirtual, Seq(methodRefIndex))

  override def getInstructionStackSizeModification(constantPool: ConstantPool, instruction: MetaObject, state: TransformationState): Int =
    super.getInstructionStackSizeModification(constantPool, instruction, state) - 1

  override def getInstructionByteCode(instruction: MetaObject): Seq[Byte] = {
    val arguments = ByteCodeSkeleton.getInstructionArguments(instruction)
    hexToBytes("b6") ++ shortToBytes(arguments(0))
  }

  override def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject, state: TransformationState) = {
    val methodRef = getInvokeTargetMethodRef(instruction, constantPool)
    val nameAndType = constantPool.getValue(ByteCodeSkeleton.getMethodRefMethodNameIndex(methodRef)).asInstanceOf[MetaObject]
    val classRef = constantPool.getValue(ByteCodeSkeleton.getMethodRefClassRefIndex(methodRef)).asInstanceOf[MetaObject]
    val className = constantPool.getValue(ByteCodeSkeleton.getClassRefName(classRef)).asInstanceOf[QualifiedClassName]
    val classType = ObjectTypeC.objectType(className)
    val descriptor = constantPool.getValue(ByteCodeSkeleton.getNameAndTypeType(nameAndType)).asInstanceOf[MetaObject]
    val (ins, outs) = getMethodStackModification(descriptor, constantPool, state)
    (Seq(classType) ++ ins, outs)
  }

  object InvokeVirtual

}
