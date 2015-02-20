package transformations.bytecode.simpleBytecode

import core.transformation.{MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.ByteCodeSkeleton.JumpBehavior

class StackLayoutAnalysisFromState(state: TransformationState, instructions: Seq[MetaObject]) {
  val initialStack = Seq[MetaObject]()
  val constantPool = ByteCodeSkeleton.getConstantPool(state.program)
  val instructionSignatureRegistry = ByteCodeSkeleton.getInstructionSignatureRegistry(state)
  val jumpBehaviorRegistry = ByteCodeSkeleton.getState(state).jumpBehaviorRegistry
  val getJumpBehavior: (Any) => JumpBehavior = clazz => jumpBehaviorRegistry(clazz)
  val getInstructionSignature = (inputTypes: Seq[MetaObject], instruction: MetaObject) =>
    instructionSignatureRegistry(instruction.clazz)(constantPool, instruction, inputTypes)
  val stackAnalysis: StackLayoutAnalysis = new StackLayoutAnalysis(instructions, getInstructionSignature, getJumpBehavior)
  val inputsPerInstructionIndex = stackAnalysis.run(0, initialStack)
}
