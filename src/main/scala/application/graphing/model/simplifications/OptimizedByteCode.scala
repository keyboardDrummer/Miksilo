package application.graphing.model.simplifications

import core.deltas.Contract
import deltas.bytecode.additions.PoptimizeC
import deltas.bytecode.extraBooleanInstructions.OptimizeComparisonInstructionsDelta
import deltas.javac.expressions.ExpressionSkeleton

object OptimizedByteCode extends TransformationGroup {
  override def dependants: Set[Contract] = Set(ExpressionSkeleton)

  override def dependencies: Set[Contract] = Set(OptimizeComparisonInstructionsDelta, PoptimizeC)
}
