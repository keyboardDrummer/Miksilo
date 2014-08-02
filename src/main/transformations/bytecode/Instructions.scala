package transformations.bytecode

import core.transformation.MetaObject
import transformations.bytecode.ByteCode._
import transformations.bytecode.LabelledJumps.LabelKey
import transformations.javac.base.ConstantPool
import transformations.javac.base.model.{JavaTypes, QualifiedClassName}

object Instructions {

  def getInstructionInputTypes(constantPool: ConstantPool, instruction: MetaObject): Seq[Any] =
    getInstructionInAndOutputs(constantPool, instruction)._1

  def getInstructionInAndOutputs(constantPool: ConstantPool, instruction: MetaObject): (Seq[Any], Seq[Any]) =
    instruction.clazz match {
      case ByteCode.IfIntegerCompareGreater => (Seq(JavaTypes.intType, JavaTypes.intType), Seq())
      case ByteCode.IfZeroKey => (Seq(JavaTypes.intType), Seq())
      case AddIntegersKey => binary(JavaTypes.intType)
      case SubtractInteger => binary(JavaTypes.intType)
      case LabelKey => (Seq(), Seq())
      case IntegerLoad => (Seq(), Seq(JavaTypes.intType))
      case IntegerStore => (Seq(JavaTypes.intType), Seq())
      case IntegerConstantKey => (Seq(), Seq(JavaTypes.intType))
      case AddressLoad => (Seq(), Seq(JavaTypes.intType))
      case AddressStore => (Seq(JavaTypes.intType), Seq())
      case IntegerReturn => (Seq(JavaTypes.intType), Seq())
      case GetStatic => (Seq(), Seq(getGetStaticReturnType(constantPool, instruction)))
      case GoToKey => (Seq(), Seq())
      case InvokeSpecial => getInvokeStackModification(constantPool, instruction)
      case InvokeVirtual => getInvokeVirtualModification(constantPool, instruction)
      case InvokeStaticKey => getInvokeStackModification(constantPool, instruction)
      case VoidReturn => (Seq(), Seq())
      case PushNull => (Seq(), Seq(JavaTypes.intType))
    }

  private def binary(_type: Any) = (Seq(_type, _type), Seq(_type))

  def getGetStaticReturnType(constantPool: ConstantPool, getStatic: MetaObject): Any = {
    val location = ByteCode.getInstructionArguments(getStatic)(0)
    val fieldRef = constantPool.getValue(location).asInstanceOf[MetaObject]
    val nameAndType = constantPool.getValue(ByteCode.getFieldRefNameAndTypeIndex(fieldRef)).asInstanceOf[MetaObject]
    val fieldType = constantPool.getValue(ByteCode.getNameAndTypeType(nameAndType)).asInstanceOf[MetaObject]
    fieldType
  }

  def getInvokeVirtualModification(constantPool: ConstantPool, instruction: MetaObject): (Seq[Any], Seq[Any]) = {
    val methodRef = getInvokeTargetMethodRef(instruction, constantPool)
    val nameAndType = constantPool.getValue(ByteCode.getMethodRefMethodNameIndex(methodRef)).asInstanceOf[MetaObject]
    val classRef = constantPool.getValue(ByteCode.getMethodRefClassRefIndex(methodRef)).asInstanceOf[MetaObject]
    val className = constantPool.getValue(ByteCode.getClassRefName(classRef)).asInstanceOf[QualifiedClassName]
    val classType = JavaTypes.objectType(className)
    val descriptor = constantPool.getValue(ByteCode.getNameAndTypeType(nameAndType)).asInstanceOf[MetaObject]
    val (ins, outs) = getMethodStackModification(descriptor)
    (Seq(classType) ++ ins, outs)
  }

  def getInvokeTargetMethodRef(instruction: MetaObject, constantPool: ConstantPool) = {
    val location = ByteCode.getInstructionArguments(instruction)(0)
    constantPool.getValue(location).asInstanceOf[MetaObject]
  }

  def getMethodStackModification(descriptor: MetaObject): (Seq[Any], Seq[Any]) = {
    val ins = ByteCode.getMethodDescriptorParameters(descriptor).map(JavaTypes.javaTypeToByteCodeType)
    val outs = Seq(JavaTypes.javaTypeToByteCodeType(ByteCode.getMethodDescriptorReturnType(descriptor)))
    (ins, outs)
  }

  def getInvokeStackModification(constantPool: ConstantPool, instruction: MetaObject): (Seq[Any], Seq[Any]) = {
    val methodRef = getInvokeTargetMethodRef(instruction, constantPool)
    val nameAndType = constantPool.getValue(ByteCode.getMethodRefMethodNameIndex(methodRef)).asInstanceOf[MetaObject]
    val descriptor = constantPool.getValue(ByteCode.getNameAndTypeType(nameAndType)).asInstanceOf[MetaObject]
    getMethodStackModification(descriptor)
  }

  def getInstructionOutputTypes(constantPool: ConstantPool, instruction: MetaObject): Seq[Any] =
    getInstructionInAndOutputs(constantPool, instruction)._2
}
