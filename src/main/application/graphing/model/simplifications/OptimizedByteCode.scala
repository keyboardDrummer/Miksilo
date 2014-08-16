package application.graphing.model.simplifications

import core.transformation.Contract
import transformations.bytecode.PoptimizeC
import transformations.bytecode.extraBooleanInstructions.OptimizeBooleanInstructionsC
import transformations.javac.expressions.ExpressionC

object OptimizedByteCode extends TransformationGroup {
  override def dependants: Set[Contract] = Set(ExpressionC)

  override def dependencies: Set[Contract] = Set(OptimizeBooleanInstructionsC, PoptimizeC)
}
