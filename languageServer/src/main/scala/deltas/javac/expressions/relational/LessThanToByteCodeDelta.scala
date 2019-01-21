package deltas.javac.expressions.relational

import deltas.bytecode.extraBooleanInstructions.LessThanInstructionDelta
import deltas.expression.relational.LessThanDelta

object LessThanToByteCodeDelta extends ComparisonOperatorToByteCodeDelta {

  override def instruction = LessThanInstructionDelta.lessThanInstruction

  override def shape = LessThanDelta.Shape

  override def description = "Converts < to bytecode"

  override def dependencies = Set(LessThanInstructionDelta)
}
