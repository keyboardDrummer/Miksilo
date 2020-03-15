package miksilo.modularLanguages.deltas.bytecode.extraBooleanInstructions

import miksilo.modularLanguages.core.node.{Node, NodeShape}
import miksilo.modularLanguages.core.deltas.Contract
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.deltas.bytecode.attributes.CodeAttributeDelta
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.integers.SmallIntegerConstantDelta
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.integers.integerCompare.IfZeroDelta
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.{InferredStackFrames, LabelDelta, LabelledLocations}

object NotInstructionDelta extends ExpandInstruction {

  def not = CodeAttributeDelta.instruction(NotInstructionKey)

  override def dependencies: Set[Contract] = super.dependencies ++ Set(LabelledLocations, IfZeroDelta)

  object NotInstructionKey extends NodeShape

  override val shape = NotInstructionKey

  override def expand(instruction: Node, methodInfo: Node, state: Language): Seq[Node] = {
    val falseStartLabel = LabelDelta.getUniqueLabel("falseStart", methodInfo)
    val endLabel = LabelDelta.getUniqueLabel("end", methodInfo)
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
