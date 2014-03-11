package languages.bytecode

import transformation.{TransformationState, MetaObject, ProgramTransformation}
import languages.javac.base.JavaBase
import scala.collection.mutable
import languages.bytecode.ByteCode._
import languages.bytecode.ByteCodeGoTo.LabelKey
import javaBytecode.MethodInfo
import util.DataFlowAnalysis

object NoMaxStack extends ProgramTransformation {
  override def dependencies: Set[ProgramTransformation] = Set(ByteCodeGoTo)

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    val clazz = program
    val constantPool = ByteCode.getConstantPool(clazz)
    for (method <- ByteCode.getMethods(clazz)) {
      val code = ByteCode.getMethodAttributes(method).find(a => a.clazz == ByteCode.CodeKey).get
      code(ByteCode.CodeMaxStackKey) = getMaxStack(constantPool, code)
    }
  }

  def getMaxStack(constantPool: Seq[Any], code: MetaObject): Integer = {
    val instructions = ByteCode.getCodeInstructions(code)
    val currentStacks = new StackSizeAnalysis(constantPool,instructions).run(0,0)
    val sortedIns = Range(0, instructions.length - 1).map(i => currentStacks(i))
    val maxStack = currentStacks.values.max
    maxStack
  }

  class StackSizeAnalysis(constantPool: Seq[Any], instructions: Seq[MetaObject])
    extends DataFlowAnalysis[Int, Int] {
    val labels = instructions.zipWithIndex.filter(i => i._1.clazz == ByteCodeGoTo.LabelKey)
      .map(p => (ByteCodeGoTo.getLabelName(p._1), p._2)).toMap

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

    override def getOutgoingNodes(instructionIndex: Int): Set[Int] = {
      val instruction = instructions(instructionIndex)
      instruction.clazz match {
        case ByteCode.GoToKey => Set(labels(ByteCodeGoTo.getGoToTarget(instruction)))
        case ByteCode.IfZeroKey =>
          val target = ByteCodeGoTo.getIfIntegerCompareGreaterTarget(instruction)
          Set(instructionIndex + 1, labels(target))
        case ByteCode.IfIntegerCompareGreater => {
          val target = ByteCodeGoTo.getIfIntegerCompareGreaterTarget(instruction)
          Set(instructionIndex + 1, labels(target))
        }
        case ByteCode.VoidReturn => Set()
        case ByteCode.IntegerReturn => Set()
        case _ => Set(instructionIndex + 1)
      }
    }
  }

  def getMethodStackModification(descriptor: MetaObject): Integer = {
    var result = JavaBase.getTypeSize(ByteCode.getMethodDescriptorReturnType(descriptor))
    for (parameter <- ByteCode.getMethodDescriptorParameters(descriptor))
      result -= JavaBase.getTypeSize(parameter)
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
}
