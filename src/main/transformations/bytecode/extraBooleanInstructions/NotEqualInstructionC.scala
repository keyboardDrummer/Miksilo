package transformations.bytecode.extraBooleanInstructions

import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.LabelledTargets
import transformations.bytecode.coreInstructions.integers.IntegerConstantC
import transformations.bytecode.coreInstructions.integers.integerCompare.IfZeroC
import transformations.bytecode.simpleBytecode.InferredStackFrames

object NotEqualInstructionC extends ExpandInstruction {

  def notEqual = instruction(NotEqualInstructionKey)

  override def dependencies: Set[Contract] = Set(LabelledTargets, IfZeroC)

  object NotEqualInstructionKey

  override def key: Any = NotEqualInstructionKey

  override def expand(instruction: MetaObject, state: TransformationState): Seq[MetaObject] = {
    val falseStartLabel = state.getUniqueLabel("falseStart")
    val endLabel = state.getUniqueLabel("end")
    Seq(LabelledTargets.ifZero(falseStartLabel),
      IntegerConstantC.integerConstant(0),
      LabelledTargets.goTo(endLabel),
      InferredStackFrames.label(falseStartLabel),
      IntegerConstantC.integerConstant(1),
      InferredStackFrames.label(endLabel))
  }
}
