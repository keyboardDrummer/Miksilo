package application.graphing.model.simplifications

import core.deltas.Contract
import deltas.bytecode.additions.PoptimizeDelta
import deltas.bytecode.extraBooleanInstructions.OptimizeComparisonInstructionsDelta
import deltas.expression.ExpressionDelta

object OptimizedByteCode extends DeltaGroup {
  override def dependants: Set[Contract] = Set(ExpressionDelta)

  override def dependencies: Set[Contract] = Set(OptimizeComparisonInstructionsDelta, PoptimizeDelta)
}
