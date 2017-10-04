package transformations.bytecode.extraBooleanInstructions

import core.particles.node.{Node, NodeClass}
import core.particles.{Contract, Language}
import transformations.bytecode.additions.LabelledLocations
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.integers.SmallIntegerConstantDelta
import transformations.bytecode.coreInstructions.integers.integerCompare.IfZeroDelta
import transformations.bytecode.simpleBytecode.InferredStackFrames

object NotInstructionC extends ExpandInstruction {

  def not = CodeAttribute.instruction(NotInstructionKey)

  override def dependencies: Set[Contract] = super.dependencies ++ Set(LabelledLocations, IfZeroDelta)

  object NotInstructionKey extends NodeClass

  override val key = NotInstructionKey

  override def expand(instruction: Node, methodInfo: Node, state: Language): Seq[Node] = {
    val falseStartLabel = LabelledLocations.getUniqueLabel("falseStart", methodInfo, state)
    val endLabel = LabelledLocations.getUniqueLabel("end", methodInfo, state)
    Seq(LabelledLocations.ifZero(falseStartLabel),
      SmallIntegerConstantDelta.integerConstant(0),
      LabelledLocations.goTo(endLabel),
      InferredStackFrames.label(falseStartLabel),
      SmallIntegerConstantDelta.integerConstant(1),
      InferredStackFrames.label(endLabel))
  }

  override def description: String = "Defines a custom instruction which applies a boolean not operation to the top stack value. " +
    "More explicitly: zero becomes one and other numbers become zero."

  override def grammarName = "ine"
}
