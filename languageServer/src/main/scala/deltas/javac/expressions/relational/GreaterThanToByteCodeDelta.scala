package deltas.javac.expressions.relational

import deltas.bytecode.extraBooleanInstructions.GreaterThanInstructionDelta

object GreaterThanToByteCodeDelta extends ComparisonOperatorToByteCodeDelta {
  override def instruction = GreaterThanInstructionDelta.greaterThanInstruction

  override def description = "Converts > to bytecode."

  override def dependencies = Set(GreaterThanInstructionDelta)

  override def shape = GreaterThanDelta.Shape

  override val base = GreaterThanDelta
}
