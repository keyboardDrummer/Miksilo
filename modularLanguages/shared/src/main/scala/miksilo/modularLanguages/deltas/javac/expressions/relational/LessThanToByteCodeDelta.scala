package miksilo.modularLanguages.deltas.javac.expressions.relational

import miksilo.modularLanguages.deltas.bytecode.extraBooleanInstructions.LessThanInstructionDelta
import miksilo.modularLanguages.deltas.expression.relational.LessThanDelta

object LessThanToByteCodeDelta extends ComparisonOperatorToByteCodeDelta {

  override def instruction = LessThanInstructionDelta.lessThanInstruction

  override def shape = LessThanDelta.Shape

  override def description = "Converts < to bytecode"

  override def dependencies = Set(LessThanInstructionDelta)
}
