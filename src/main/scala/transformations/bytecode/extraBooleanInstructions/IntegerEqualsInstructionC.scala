package transformations.bytecode.extraBooleanInstructions

import core.particles.node.{Key, Node}
import core.particles.{CompilationState, Contract}
import transformations.bytecode.additions.LabelledTargets
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.integers.SmallIntegerConstantC
import transformations.bytecode.coreInstructions.integers.integerCompare.IfIntegerCompareEqualC
import transformations.bytecode.simpleBytecode.InferredStackFrames

object IntegerEqualsInstructionC extends ExpandInstruction {

  def equals = CodeAttribute.instruction(IntegerEqualsInstructionKey)

  override def dependencies: Set[Contract] = super.dependencies ++ Set(LabelledTargets, IfIntegerCompareEqualC)

  override val key = IntegerEqualsInstructionKey

  override def expand(instruction: Node, state: CompilationState): Seq[Node] = {
    val falseStartLabel = state.getUniqueLabel("falseStart")
    val endLabel = state.getUniqueLabel("end")
    Seq(LabelledTargets.ifIntegerCompareEquals(falseStartLabel),
      SmallIntegerConstantC.integerConstant(0),
      LabelledTargets.goTo(endLabel),
      InferredStackFrames.label(falseStartLabel),
      SmallIntegerConstantC.integerConstant(1),
      InferredStackFrames.label(endLabel))
  }

  object IntegerEqualsInstructionKey extends Key

  override def description: String = "Defines a custom instruction which applies == to the top stack values."
}
