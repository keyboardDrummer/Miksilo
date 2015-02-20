package transformations.bytecode.simpleBytecode

import core.transformation.sillyCodePieces.ParticleWithPhase
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton
import transformations.bytecode.additions.LabelledTargets
import transformations.bytecode.additions.LabelledTargets.LabelKey
import transformations.bytecode.attributes.CodeAttribute
import transformations.types.TypeC

object InferredMaxStack extends ParticleWithPhase {
  override def dependencies: Set[Contract] = Set(LabelledTargets)

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    val clazz = program

    def getMaxStack(code: MetaObject): Integer = {
      val instructions = CodeAttribute.getCodeInstructions(code)
      val stackLayoutAnalysis = new StackLayoutAnalysisFromState(state, instructions)

      val maxStack = stackLayoutAnalysis.inputsPerInstructionIndex.values.map(
        stackLayout => stackLayout.map(_type => TypeC.getTypeSize(_type,state)).sum).max
      maxStack
    }

    for (method <- ByteCodeSkeleton.getMethods(clazz)) {
      val code = ByteCodeSkeleton.getMethodAttributes(method).find(a => a.clazz == CodeAttribute.CodeKey).get
      code(CodeAttribute.CodeMaxStackKey) = getMaxStack(code)
    }
  }


  def getInstructionStackSizeModification(constantPool: Seq[Any], instruction: MetaObject): Integer =
    instruction.clazz match {
      case LabelKey => 0
    }
}
