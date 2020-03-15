package miksilo.modularLanguages.deltas.javac.expressions.relational

import miksilo.modularLanguages.deltas.bytecode.extraBooleanInstructions.GreaterThanInstructionDelta
import miksilo.modularLanguages.deltas.expression.relational.GreaterThanDelta

object GreaterThanToByteCodeDelta extends ComparisonOperatorToByteCodeDelta {
  override def instruction = GreaterThanInstructionDelta.greaterThanInstruction

  override def description = "Converts > to bytecode."

  override def dependencies = Set(GreaterThanInstructionDelta)

  override def shape = GreaterThanDelta.Shape
}
