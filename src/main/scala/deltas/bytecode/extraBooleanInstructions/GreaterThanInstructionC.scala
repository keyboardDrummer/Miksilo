package deltas.bytecode.extraBooleanInstructions

import core.deltas.node.{Node, NodeClass}
import core.deltas.{Contract, Language}
import deltas.bytecode.attributes.CodeAttribute
import deltas.bytecode.coreInstructions.integers.SmallIntegerConstantDelta
import deltas.bytecode.coreInstructions.integers.integerCompare.IfIntegerCompareGreaterOrEqualDelta
import deltas.bytecode.simpleBytecode.{InferredStackFrames, LabelledLocations}

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
