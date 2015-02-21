package transformations.bytecode.simpleBytecode

import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton.JumpBehavior
import transformations.bytecode.attributes.CodeAttribute
import transformations.javac.classes.QualifiedClassName
import transformations.types.ObjectTypeC

class InstructionTypeAnalysisFromState(state: TransformationState, method: MetaObject) {
  val constantPool = ByteCodeSkeleton.getConstantPool(state.program)

  val typeAnalysis = getTypeAnalysis
  val parameters = getMethodParameters
  val initialVariables = parameters.zipWithIndex.map(p => p._2 -> p._1).toMap
  val initialStack = Seq[MetaObject]()
  val initialProgramTypeState: ProgramTypeState = ProgramTypeState(initialStack, initialVariables)
  val typeStatePerInstruction = typeAnalysis.run(0, initialProgramTypeState)

  def getTypeAnalysis = {
    val codeAnnotation = ByteCodeSkeleton.getMethodAttributes(method).find(a => a.clazz == CodeAttribute.CodeKey).get
    val instructions = CodeAttribute.getCodeInstructions(codeAnnotation)
    val instructionSignatureRegistry = ByteCodeSkeleton.getInstructionSignatureRegistry(state)
    val jumpBehaviorRegistry = ByteCodeSkeleton.getState(state).jumpBehaviorRegistry
    val getJumpBehavior: (Any) => JumpBehavior = clazz => jumpBehaviorRegistry(clazz)
    val getInstructionSignature = (inputTypes: ProgramTypeState, instruction: MetaObject) =>
      instructionSignatureRegistry(instruction.clazz)(constantPool, instruction, inputTypes)
    val instructionVariableUpdateRegistry = ByteCodeSkeleton.getState(state).localUpdates
    val getVariableUpdates: (MetaObject) => Map[Int, MetaObject] = instruction => instructionVariableUpdateRegistry(instruction.clazz)(instruction)
    val typeAnalysis: InstructionTypeAnalysis = new InstructionTypeAnalysis(instructions, getVariableUpdates, getInstructionSignature, getJumpBehavior)
    typeAnalysis
  }
  
  def getMethodParameters = {
    val methodIsStatic: Boolean = ByteCodeSkeleton.getMethodAccessFlags(method).contains(ByteCodeSkeleton.StaticAccess)
    val methodDescriptor = constantPool.getValue(ByteCodeSkeleton.getMethodDescriptorIndex(method)).asInstanceOf[MetaObject]
    val methodParameters = ByteCodeSkeleton.getMethodDescriptorParameters(methodDescriptor)
    if (methodIsStatic) {
      methodParameters
    }
    else {
      val clazz = state.program
      val clazzRefIndex = clazz(ByteCodeSkeleton.ClassNameIndexKey).asInstanceOf[Int]
      val clazzRef = constantPool.getValue(clazzRefIndex).asInstanceOf[MetaObject]
      val className = constantPool.getValue(ByteCodeSkeleton.getClassRefName(clazzRef)).asInstanceOf[QualifiedClassName]
      Seq(ObjectTypeC.objectType(className)) ++ methodParameters
    }
  }
}
