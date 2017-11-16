package deltas.bytecode.coreInstructions

import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{Node, NodeField}
import core.deltas.{Compilation, Language}
import deltas.bytecode.ByteCodeSkeleton
import deltas.bytecode.constants.MethodRefConstant.MethodRefWrapper
import deltas.bytecode.constants._
import deltas.bytecode.extraConstants.QualifiedClassNameConstantDelta
import deltas.bytecode.simpleBytecode.ProgramTypeState
import deltas.bytecode.types.{ObjectTypeDelta, TypeSkeleton}
import deltas.javac.types.MethodType._

abstract class InvokeDelta extends InstructionDelta {

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: Compilation): InstructionSignature = {
    val methodRef = getInvokeTargetMethodRef(instruction)
    val nameAndType = methodRef.nameAndType
    val descriptor = nameAndType._type.value
    getMethodStackModification(descriptor, state)
  }

  def getInstanceInstructionSignature(instruction: Node, typeState: ProgramTypeState, state: Compilation): InstructionSignature = {
    val methodRef = getInvokeTargetMethodRef(instruction)
    val nameAndType = methodRef.nameAndType
    val classRef = methodRef(MethodRefConstant.ClassRef).asInstanceOf[Node]
    val className = classRef(ClassInfoConstant.Name).asInstanceOf[Node]
    val classType = ObjectTypeDelta.objectType(QualifiedClassNameConstantDelta.get(className))
    val descriptor = nameAndType._type.value
    val InstructionSignature(ins, outs) = getMethodStackModification(descriptor, state)
    InstructionSignature(Seq(classType) ++ ins, outs)
  }

  object MethodRef extends NodeField
  override def inject(state: Language): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getRegistry(state).constantReferences.put(key, Map(MethodRef -> MethodRefConstant.key))
  }

  override def argumentsGrammar(grammars: LanguageGrammars) = {
    import grammars._
    find(ConstantPoolIndexGrammar).as(MethodRef)
  }

  def getMethodStackModification(methodType: Node, state: Compilation): InstructionSignature = {
    val ins = methodType.parameterTypes.map(_type => TypeSkeleton.toStackType(_type, state))
    val outs = Seq(TypeSkeleton.toStackType(methodType.returnType, state))
    InstructionSignature(ins, outs.filter(p => TypeSkeleton.getTypeSize(p, state) > 0))
  }

  def getInvokeTargetMethodRef(instruction: Node): MethodRefWrapper[Node] = {
    instruction(MethodRef).asInstanceOf[Node]
  }

  override def getInstructionSize: Int = 3
}
