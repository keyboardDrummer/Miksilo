package transformations.bytecode.extraBooleanInstructions

import core.particles.node.{Key, Node}
import core.particles.{CompilationState, Contract}
import transformations.bytecode.additions.LabelledLocations
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.integers.SmallIntegerConstantDelta
import transformations.bytecode.coreInstructions.integers.integerCompare.IfIntegerCompareLessDelta
import transformations.bytecode.simpleBytecode.InferredStackFrames

object LessThanInstructionC extends ExpandInstruction {

  def lessThanInstruction = CodeAttribute.instruction(LessThanInstructionKey)

  override def dependencies: Set[Contract] = super.dependencies ++ Set(LabelledLocations, IfIntegerCompareLessDelta)

  override val key = LessThanInstructionKey

  override def expand(instruction: Node, state: CompilationState): Seq[Node] = {
    val trueLabel = state.getUniqueLabel("true")
    val endLabel = state.getUniqueLabel("end")
    Seq(LabelledLocations.ifIntegerCompareLess(trueLabel),
      SmallIntegerConstantDelta.integerConstant(0),
      LabelledLocations.goTo(endLabel),
      InferredStackFrames.label(trueLabel),
      SmallIntegerConstantDelta.integerConstant(1),
      InferredStackFrames.label(endLabel))
  }

  object LessThanInstructionKey extends Key

  override def description: String = "Defines a custom instruction which applies < to the top stack values."

  override def grammarName = "ilt"
}
