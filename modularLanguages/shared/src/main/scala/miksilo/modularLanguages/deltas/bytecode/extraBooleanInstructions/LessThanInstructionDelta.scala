package miksilo.modularLanguages.deltas.bytecode.extraBooleanInstructions

import miksilo.modularLanguages.core.node.{Node, NodeShape}
import miksilo.modularLanguages.core.deltas.Contract
import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.deltas.bytecode.attributes.CodeAttributeDelta
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.integers.SmallIntegerConstantDelta
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.integers.integerCompare.IfIntegerCompareLessDelta
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.{InferredStackFrames, LabelDelta, LabelledLocations}

object LessThanInstructionDelta extends ExpandInstruction {

  def lessThanInstruction = CodeAttributeDelta.instruction(LessThanInstructionKey)

  override def dependencies: Set[Contract] = super.dependencies ++ Set(LabelledLocations, IfIntegerCompareLessDelta)

  override val shape = LessThanInstructionKey

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
