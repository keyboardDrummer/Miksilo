package deltas.bytecode.extraBooleanInstructions

import core.deltas.node.{Node, NodeClass}
import core.deltas.{Contract, Language}
import deltas.bytecode.attributes.CodeAttribute
import deltas.bytecode.coreInstructions.integers.SmallIntegerConstantDelta
import deltas.bytecode.coreInstructions.integers.integerCompare.IfIntegerCompareEqualDelta
import deltas.bytecode.simpleBytecode.{InferredStackFrames, LabelledLocations}

object IntegerEqualsInstructionC extends ExpandInstruction {

  def equals = CodeAttribute.instruction(IntegerEqualsInstructionKey)

  override def dependencies: Set[Contract] = super.dependencies ++ Set(LabelledLocations, IfIntegerCompareEqualDelta)

  override val key = IntegerEqualsInstructionKey

  override def expand(instruction: Node, methodInfo: Node, state: Language): Seq[Node] = {
    val falseStartLabel = LabelledLocations.getUniqueLabel("falseStart", methodInfo, state)
    val endLabel = LabelledLocations.getUniqueLabel("end", methodInfo, state)
    Seq(LabelledLocations.ifIntegerCompareEquals(falseStartLabel),
      SmallIntegerConstantDelta.integerConstant(0),
      LabelledLocations.goTo(endLabel),
      InferredStackFrames.label(falseStartLabel),
      SmallIntegerConstantDelta.integerConstant(1),
      InferredStackFrames.label(endLabel))
  }

  object IntegerEqualsInstructionKey extends NodeClass

  override def description: String = "Defines a custom instruction which applies == to the top stack values."

  override def grammarName = "ieq"
}
