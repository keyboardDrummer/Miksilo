package application.graphing.model.simplifications

import core.deltas.Contract
import deltas.bytecode.additions.PoptimizeDelta
import deltas.bytecode.extraBooleanInstructions.OptimizeComparisonInstructionsDelta
import deltas.javac.expressions.ExpressionSkeleton

object OptimizedByteCode extends DeltaGroup {
  override def dependants: Set[Contract] = Set(ExpressionSkeleton)

  override def dependencies: Set[Contract] = Set(OptimizeComparisonInstructionsDelta, PoptimizeDelta)
}
