package transformations.bytecode

import core.transformation.MetaObject
import util.DataFlowAnalysis
import transformations.bytecode.ByteCode._
import transformations.bytecode.LabelledJumps.LabelKey
import transformations.javac.base.JavaBase

class StackSizeAnalysis(constantPool: Seq[Any], instructions: Seq[MetaObject])
  extends InstructionFlowAnalysis[Int](instructions) {

  override def combineState(first: Int, second: Int): Int = {
    if (first == second)
      return first

    throw new RuntimeException()
  }

  override def updateState(state: Int, instructionIndex: Int): Int = {
    val instruction = instructions(instructionIndex)
    val result = state + getInstructionStackSizeModification(constantPool, instruction)
    if (result < 0)
      throw new RuntimeException()
    result
  }

  def getInstructionStackSizeModification(constantPool: Seq[Any], instruction: MetaObject): Integer =
    instruction.clazz match {
      case AddIntegersKey => -1
      case SubtractInteger => -1
      case LabelKey => 0
      case IntegerLoad => 1
      case IntegerConstantKey => 1
      case AddressLoad => 1
      case IntegerReturn => -1
      case GetStatic => 1
      case GoToKey => 0
      case InvokeSpecial => getInvokeStackModification(constantPool, instruction)
      case InvokeVirtual => getInvokeStackModification(constantPool, instruction) + 1
      case InvokeStaticKey => getInvokeStackModification(constantPool, instruction)
      case IfIntegerCompareGreater => -2
      case IfZeroKey => -1
      case VoidReturn => 0
    }

  def getInvokeStackModification(constantPool: Seq[Any], instruction: MetaObject): Integer = {
    val location = ByteCode.getInstructionArguments(instruction)(0)
    val methodRef = ByteCode.constantPoolGet(constantPool, location).asInstanceOf[MetaObject]
    val nameAndType = ByteCode.constantPoolGet(constantPool, ByteCode.getMethodRefMethodNameIndex(methodRef)).asInstanceOf[MetaObject]
    val descriptor = ByteCode.constantPoolGet(constantPool, ByteCode.getNameAndTypeType(nameAndType)).asInstanceOf[MetaObject]
    getMethodStackModification(descriptor)
  }

  def getMethodStackModification(descriptor: MetaObject): Integer = {
    var result = JavaBase.getTypeSize(ByteCode.getMethodDescriptorReturnType(descriptor))
    for (parameter <- ByteCode.getMethodDescriptorParameters(descriptor))
      result -= JavaBase.getTypeSize(parameter)
    result
  }
}
