package transformations.bytecode.coreInstructions

import core.particles.CompilationState
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeField}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.constants._
import transformations.bytecode.simpleBytecode.ProgramTypeState
import transformations.bytecode.types.{ObjectTypeC, TypeSkeleton}
import transformations.javac.classes.ConstantPool
import transformations.javac.types.MethodTypeC._

abstract class InvokeC extends InstructionC {

  override def getSignature(instruction: Node, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = {
    val constantPool: ConstantPool = state.program.constantPool
    val methodRef = getInvokeTargetMethodRef(instruction, constantPool)
    val nameAndType = constantPool.getValue(MethodRefConstant.getNameAndTypeIndex(methodRef)).asInstanceOf[Node]
    val descriptor = constantPool.getValue(NameAndType.getTypeIndex(nameAndType)).asInstanceOf[Node]
    getMethodStackModification(descriptor, constantPool, state)
  }

  def getInstanceInstructionSignature(instruction: Node, typeState: ProgramTypeState, state: CompilationState): InstructionSignature = {
    val constantPool = state.program.constantPool
    val methodRef = getInvokeTargetMethodRef(instruction, constantPool)
    val nameAndType = constantPool.getValue(MethodRefConstant.getNameAndTypeIndex(methodRef)).asInstanceOf[Node]
    val classRef = constantPool.getValue(MethodRefConstant.getMethodRefClassRefIndex(methodRef)).asInstanceOf[Node]
    val className = constantPool.getValue(ClassRefConstant.getNameIndex(classRef)).asInstanceOf[Node]
    val classType = ObjectTypeC.objectType(QualifiedClassNameConstant.get(className))
    val descriptor = constantPool.getValue(NameAndType.getTypeIndex(nameAndType)).asInstanceOf[Node]
    val InstructionSignature(ins, outs) = getMethodStackModification(descriptor, constantPool, state)
    InstructionSignature(Seq(classType) ++ ins, outs)
  }

  object MethodRef extends NodeField
  override def inject(state: CompilationState): Unit = {
    super.inject(state)
    ByteCodeSkeleton.getState(state).constantReferences.put(key, Map(MethodRef -> MethodRefConstant.key))
  }

  override def argumentsGrammar(grammars: GrammarCatalogue) = grammars.find(ConstantPoolIndexGrammar).as(MethodRef)

  def getMethodStackModification(methodType: Node, constantPool: ConstantPool, state: CompilationState): InstructionSignature = {
    val ins = methodType.parameterTypes.map(_type => TypeSkeleton.toStackType(_type, state))
    val outs = Seq(TypeSkeleton.toStackType(methodType.returnType, state))
    InstructionSignature(ins, outs.filter(p => TypeSkeleton.getTypeSize(p, state) > 0))
  }

  def getInvokeTargetMethodRef(instruction: Node, constantPool: ConstantPool): Node = {
    val location = instruction(MethodRef).asInstanceOf[Int]
    constantPool.getValue(location).asInstanceOf[Node]
  }

  override def getInstructionSize: Int = 3
}
