package miksilo.playground.application.graphing.model.simplifications

import miksilo.modularLanguages.core.deltas.Contract
import miksilo.modularLanguages.deltas.bytecode.additions.PoptimizeDelta
import miksilo.modularLanguages.deltas.bytecode.extraBooleanInstructions.OptimizeComparisonInstructionsDelta
import miksilo.modularLanguages.deltas.expression.ExpressionDelta

object OptimizedByteCode extends DeltaGroup {
  override def dependants: Set[Contract] = Set(ExpressionDelta)

  override def dependencies: Set[Contract] = Set(OptimizeComparisonInstructionsDelta, PoptimizeDelta)
}
