package deltas.bytecode.extraBooleanInstructions

import core.deltas.node.{Node, NodeShape}
import core.deltas.{Contract, Language}
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.coreInstructions.integers.SmallIntegerConstantDelta
import deltas.bytecode.coreInstructions.integers.integerCompare.IfIntegerCompareLessDelta
import deltas.bytecode.simpleBytecode.{InferredStackFrames, LabelDelta, LabelledLocations}

object LessThanInstructionDelta extends ExpandInstruction {

  def lessThanInstruction = CodeAttributeDelta.instruction(LessThanInstructionKey)

  override def dependencies: Set[Contract] = super.dependencies ++ Set(LabelledLocations, IfIntegerCompareLessDelta)

  override val key = LessThanInstructionKey

  override def expand(instruction: Node, methodInfo: Node, state: Language): Seq[Node] = {
    val trueLabel = LabelDelta.getUniqueLabel("true", methodInfo)
    val endLabel = LabelDelta.getUniqueLabel("end", methodInfo)
    Seq(LabelledLocations.ifIntegerCompareLess(trueLabel),
      SmallIntegerConstantDelta.integerConstant(0),
      LabelledLocations.goTo(endLabel),
      InferredStackFrames.label(trueLabel),
      SmallIntegerConstantDelta.integerConstant(1),
      InferredStackFrames.label(endLabel))
  }

  object LessThanInstructionKey extends NodeShape

  override def description: String = "Defines a custom instruction which applies < to the top stack values."

  override def grammarName = "ilt"
}
