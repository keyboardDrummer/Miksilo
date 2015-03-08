package application.graphing.model.simplifications

import core.particles.Contract
import transformations.bytecode.extraBooleanInstructions.OptimizeBooleanInstructionsC
import transformations.bytecode.additions.PoptimizeC
import transformations.javac.expressions.ExpressionSkeleton

object OptimizedByteCode extends TransformationGroup {
  override def dependants: Set[Contract] = Set(ExpressionSkeleton)

  override def dependencies: Set[Contract] = Set(OptimizeBooleanInstructionsC, PoptimizeC)
}
