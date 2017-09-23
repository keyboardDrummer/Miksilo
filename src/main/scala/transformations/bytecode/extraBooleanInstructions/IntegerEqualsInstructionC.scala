package transformations.bytecode.extraBooleanInstructions

import core.particles.node.{Key, Node}
import core.particles.{CompilationState, Contract}
import transformations.bytecode.additions.LabelledLocations
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.integers.SmallIntegerConstantDelta$
import transformations.bytecode.coreInstructions.integers.integerCompare.IfIntegerCompareEqualDelta$
import transformations.bytecode.simpleBytecode.InferredStackFrames

object IntegerEqualsInstructionC extends ExpandInstruction {

  def equals = CodeAttribute.instruction(IntegerEqualsInstructionKey)

  override def dependencies: Set[Contract] = super.dependencies ++ Set(LabelledLocations, IfIntegerCompareEqualDelta$)

  override val key = IntegerEqualsInstructionKey

  override def expand(instruction: Node, state: CompilationState): Seq[Node] = {
    val falseStartLabel = state.getUniqueLabel("falseStart")
    val endLabel = state.getUniqueLabel("end")
    Seq(LabelledLocations.ifIntegerCompareEquals(falseStartLabel),
      SmallIntegerConstantDelta$.integerConstant(0),
      LabelledLocations.goTo(endLabel),
      InferredStackFrames.label(falseStartLabel),
      SmallIntegerConstantDelta$.integerConstant(1),
      InferredStackFrames.label(endLabel))
  }

  object IntegerEqualsInstructionKey extends Key

  override def description: String = "Defines a custom instruction which applies == to the top stack values."
}
