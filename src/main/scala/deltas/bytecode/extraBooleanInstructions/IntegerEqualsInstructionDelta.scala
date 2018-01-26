package deltas.bytecode.extraBooleanInstructions

import core.deltas.node.{Node, NodeShape}
import core.deltas.Contract
import core.language.Language
import deltas.bytecode.attributes.CodeAttributeDelta
import deltas.bytecode.coreInstructions.integers.SmallIntegerConstantDelta
import deltas.bytecode.coreInstructions.integers.integerCompare.IfIntegerCompareEqualDelta
import deltas.bytecode.simpleBytecode.{InferredStackFrames, LabelDelta, LabelledLocations}

object IntegerEqualsInstructionDelta extends ExpandInstruction {

  def equals = CodeAttributeDelta.instruction(IntegerEqualsInstructionKey)

  override def dependencies: Set[Contract] = super.dependencies ++ Set(LabelledLocations, IfIntegerCompareEqualDelta)

  override val key = IntegerEqualsInstructionKey

  override def expand(instruction: Node, methodInfo: Node, state: Language): Seq[Node] = {
    val falseStartLabel = LabelDelta.getUniqueLabel("falseStart", methodInfo)
    val endLabel = LabelDelta.getUniqueLabel("end", methodInfo)
    Seq(LabelledLocations.ifIntegerCompareEquals(falseStartLabel),
      SmallIntegerConstantDelta.integerConstant(0),
      LabelledLocations.goTo(endLabel),
      InferredStackFrames.label(falseStartLabel),
      SmallIntegerConstantDelta.integerConstant(1),
      InferredStackFrames.label(endLabel))
  }

  object IntegerEqualsInstructionKey extends NodeShape

  override def description: String = "Defines a custom instruction which applies == to the top stack values."

  override def grammarName = "ieq"
}
