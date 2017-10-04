package transformations.bytecode.extraBooleanInstructions

import core.particles.node.{Node, NodeClass}
import core.particles.{Contract, Language}
import transformations.bytecode.additions.LabelledLocations
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.integers.SmallIntegerConstantDelta
import transformations.bytecode.coreInstructions.integers.integerCompare.IfIntegerCompareGreaterOrEqualDelta
import transformations.bytecode.simpleBytecode.InferredStackFrames

object GreaterThanInstructionC extends ExpandInstruction {

  def greaterThanInstruction = CodeAttribute.instruction(GreaterThanInstructionKey)

  override def dependencies: Set[Contract] = super.dependencies ++ Set(LabelledLocations, IfIntegerCompareGreaterOrEqualDelta)

  override val key = GreaterThanInstructionKey

  override def expand(instruction: Node, methodInfo: Node, state: Language): Seq[Node] = {
    val trueLabel = LabelledLocations.getUniqueLabel("true", methodInfo, state)
    val endLabel = LabelledLocations.getUniqueLabel("end", methodInfo, state)
    Seq(LabelledLocations.ifIntegerCompareGreater(trueLabel),
      SmallIntegerConstantDelta.integerConstant(0),
      LabelledLocations.goTo(endLabel),
      InferredStackFrames.label(trueLabel),
      SmallIntegerConstantDelta.integerConstant(1),
      InferredStackFrames.label(endLabel))
  }

  object GreaterThanInstructionKey extends NodeClass

  override def description: String = "Defines a custom instruction which applies > to the top stack values."

  override def grammarName = "igt"
}
