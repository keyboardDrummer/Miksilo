package transformations.bytecode.simpleBytecode

import core.transformation.sillyCodePieces.ParticleWithPhase
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.{ByteCodeMethodInfo, ByteCodeSkeleton}
import transformations.bytecode.additions.LabelledTargets
import transformations.bytecode.additions.LabelledTargets.LabelKey
import transformations.bytecode.attributes.CodeAttribute
import transformations.types.TypeC

object InferredMaxStack extends ParticleWithPhase {
  override def dependencies: Set[Contract] = Set(LabelledTargets)

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    val clazz = program

    def getMaxStack(method: MetaObject): Integer = {
      val stackLayoutAnalysis = new InstructionTypeAnalysisFromState(state, method)

      val maxStack = stackLayoutAnalysis.typeStatePerInstruction.values.map(
        stackLayout => stackLayout.stackTypes.map(_type => TypeC.getTypeSize(_type,state)).sum).max
      maxStack
    }

    for (method <- ByteCodeSkeleton.getMethods(clazz)) {
      val code = ByteCodeMethodInfo.getMethodAttributes(method).find(a => a.clazz == CodeAttribute.CodeKey).get
      code(CodeAttribute.CodeMaxStackKey) = getMaxStack(method)
    }
  }

  def getInstructionStackSizeModification(constantPool: Seq[Any], instruction: MetaObject): Integer =
    instruction.clazz match {
      case LabelKey => 0
    }
}
