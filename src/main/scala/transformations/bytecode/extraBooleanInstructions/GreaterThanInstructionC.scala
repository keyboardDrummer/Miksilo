package transformations.bytecode.extraBooleanInstructions

import core.particles.node.{Key, Node}
import core.particles.{CompilationState, Contract}
import transformations.bytecode.additions.LabelledLocations
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.integers.SmallIntegerConstantC
import transformations.bytecode.coreInstructions.integers.integerCompare.{IfIntegerCompareGreaterOrEqualC, IfIntegerCompareLessC}
import transformations.bytecode.simpleBytecode.InferredStackFrames

object GreaterThanInstructionC extends ExpandInstruction {

  def greaterThanInstruction = CodeAttribute.instruction(GreaterThanInstructionKey)

  override def dependencies: Set[Contract] = super.dependencies ++ Set(LabelledLocations, IfIntegerCompareGreaterOrEqualC)

  override val key = GreaterThanInstructionKey

  override def expand(instruction: Node, state: CompilationState): Seq[Node] = {
    val trueLabel = state.getUniqueLabel("true")
    val endLabel = state.getUniqueLabel("end")
    Seq(LabelledLocations.ifIntegerCompareGreater(trueLabel),
      SmallIntegerConstantC.integerConstant(0),
      LabelledLocations.goTo(endLabel),
      InferredStackFrames.label(trueLabel),
      SmallIntegerConstantC.integerConstant(1),
      InferredStackFrames.label(endLabel))
  }

  object GreaterThanInstructionKey extends Key

  override def description: String = "Defines a custom instruction which applies > to the top stack values."
}
