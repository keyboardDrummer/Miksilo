package transformations.bytecode.extraBooleanInstructions

import core.particles.node.{Key, Node}
import core.particles.{CompilationState, Contract}
import transformations.bytecode.additions.LabelledTargets
import transformations.bytecode.attributes.CodeAttribute
import transformations.bytecode.coreInstructions.integers.SmallIntegerConstantC
import transformations.bytecode.coreInstructions.integers.integerCompare.IfZeroC
import transformations.bytecode.simpleBytecode.InferredStackFrames

object NotInstructionC extends ExpandInstruction {

  def not = CodeAttribute.instruction(NotInstructionKey)

  override def dependencies: Set[Contract] = super.dependencies ++ Set(LabelledTargets, IfZeroC)

  object NotInstructionKey extends Key

  override val key = NotInstructionKey

  override def expand(instruction: Node, state: CompilationState): Seq[Node] = {
    val falseStartLabel = state.getUniqueLabel("falseStart")
    val endLabel = state.getUniqueLabel("end")
    Seq(LabelledTargets.ifZero(falseStartLabel),
      SmallIntegerConstantC.integerConstant(0),
      LabelledTargets.goTo(endLabel),
      InferredStackFrames.label(falseStartLabel),
      SmallIntegerConstantC.integerConstant(1),
      InferredStackFrames.label(endLabel))
  }

  override def description: String = "Defines a custom instruction which applies a boolean not operation to the top stack value. " +
    "More explicitly: zero becomes one and other numbers become zero."
}
