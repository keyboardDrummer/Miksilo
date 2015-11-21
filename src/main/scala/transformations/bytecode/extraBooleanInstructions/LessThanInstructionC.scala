package transformations.bytecode.extraBooleanInstructions

import core.particles.node.Node
import core.particles.{CompilationState, Contract}
import transformations.bytecode.additions.LabelledTargets
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.integers.SmallIntegerConstantC
import transformations.bytecode.coreInstructions.integers.integerCompare.IfIntegerCompareLessC
import transformations.bytecode.simpleBytecode.InferredStackFrames

object LessThanInstructionC extends ExpandInstruction {

  def lessThanInstruction = CodeAttribute.instruction(LessThanInstructionKey)

  override def dependencies: Set[Contract] = super.dependencies ++ Set(LabelledTargets, IfIntegerCompareLessC)

  override def key: Any = LessThanInstructionKey

  override def expand(instruction: Node, state: CompilationState): Seq[Node] = {
    val trueLabel = state.getUniqueLabel("true")
    val endLabel = state.getUniqueLabel("end")
    Seq(LabelledTargets.ifIntegerCompareLess(trueLabel),
      SmallIntegerConstantC.integerConstant(0),
      LabelledTargets.goTo(endLabel),
      InferredStackFrames.label(trueLabel),
      SmallIntegerConstantC.integerConstant(1),
      InferredStackFrames.label(endLabel))
  }

  object LessThanInstructionKey

  override def description: String = "Defines a custom instruction which applies < to the top stack values."
}
