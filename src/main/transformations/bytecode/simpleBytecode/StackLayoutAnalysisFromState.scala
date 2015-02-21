package transformations.bytecode.simpleBytecode

import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton.JumpBehavior
import transformations.bytecode.attributes.CodeAttribute
import transformations.javac.classes.QualifiedClassName
import transformations.types.ObjectTypeC

class StackLayoutAnalysisFromState(state: TransformationState, method: MetaObject) {
  val constantPool = ByteCodeSkeleton.getConstantPool(state.program)
  
  val methodDescriptor = constantPool.getValue(ByteCodeSkeleton.getMethodDescriptorIndex(method)).asInstanceOf[MetaObject]
  val methodParameters = ByteCodeSkeleton.getMethodDescriptorParameters(methodDescriptor)
  val codeAnnotation = ByteCodeSkeleton.getMethodAttributes(method).find(a => a.clazz == CodeAttribute.CodeKey).get
  val instructions = CodeAttribute.getCodeInstructions(codeAnnotation)
  
  val initialStack = Seq[MetaObject]()
  val instructionSignatureRegistry = ByteCodeSkeleton.getInstructionSignatureRegistry(state)
  val jumpBehaviorRegistry = ByteCodeSkeleton.getState(state).jumpBehaviorRegistry
  val getJumpBehavior: (Any) => JumpBehavior = clazz => jumpBehaviorRegistry(clazz)
  val getInstructionSignature = (inputTypes: ProgramTypeState, instruction: MetaObject) =>
    instructionSignatureRegistry(instruction.clazz)(constantPool, instruction, inputTypes)
  val instructionVariableUpdateRegistry = ByteCodeSkeleton.getState(state).localUpdates
  val getVariableUpdates: (MetaObject) => Map[Int, MetaObject] = instruction => instructionVariableUpdateRegistry(instruction.clazz)(instruction)
  val stackAnalysis: StackLayoutAnalysis = new StackLayoutAnalysis(instructions, getVariableUpdates, getInstructionSignature, getJumpBehavior)

  val methodIsStatic: Boolean = ByteCodeSkeleton.getMethodAccessFlags(method).contains(ByteCodeSkeleton.StaticAccess)
  val parameters = if (methodIsStatic)
  {
    methodParameters
  }
  else
  {
    val clazz = state.program
    val clazzRefIndex = clazz(ByteCodeSkeleton.ClassNameIndexKey).asInstanceOf[Int]
    val clazzRef = constantPool.getValue(clazzRefIndex).asInstanceOf[MetaObject]
    val className = constantPool.getValue(ByteCodeSkeleton.getClassRefName(clazzRef)).asInstanceOf[QualifiedClassName]
    Seq(ObjectTypeC.objectType(className)) ++ methodParameters
  }
  val initialVariables = parameters.zipWithIndex.map(p => p._2 -> p._1).toMap

  val typeStatePerInstruction = stackAnalysis.run(0, ProgramTypeState(initialStack, initialVariables))
}
