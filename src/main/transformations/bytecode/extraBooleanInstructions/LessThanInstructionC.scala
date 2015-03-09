package transformations.bytecode.extraBooleanInstructions

import core.particles.{CompilationState, Contract, MetaObject}
import transformations.bytecode.ByteCodeSkeleton._
import transformations.bytecode.additions.LabelledTargets
import transformations.bytecode.coreInstructions.integers.IntegerConstantC
import transformations.bytecode.coreInstructions.integers.integerCompare.IfIntegerCompareLessC
import transformations.bytecode.simpleBytecode.InferredStackFrames

object LessThanInstructionC extends ExpandInstruction {

  def lessThanInstruction = instruction(LessThanInstructionKey)

  override def dependencies: Set[Contract] = super.dependencies ++ Set(LabelledTargets, IfIntegerCompareLessC)

  override def key: Any = LessThanInstructionKey

  override def expand(instruction: MetaObject, state: CompilationState): Seq[MetaObject] = {
    val falseStartLabel = state.getUniqueLabel("falseStart")
    val endLabel = state.getUniqueLabel("end")
    Seq(LabelledTargets.ifIntegerCompareLess(falseStartLabel),
      IntegerConstantC.integerConstant(0),
      LabelledTargets.goTo(endLabel),
      InferredStackFrames.label(falseStartLabel),
      IntegerConstantC.integerConstant(1),
      InferredStackFrames.label(endLabel))
  }

  object LessThanInstructionKey

  override def description: String = "Defines a custom instruction which applies < to the top stack values."
}
