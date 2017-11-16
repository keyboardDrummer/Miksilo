package application.graphing.model.simplifications

import core.deltas.Contract
import deltas.bytecode.simpleBytecode.{InferredMaxStack, InferredStackFrames}
import deltas.javac.expressions.ExpressionSkeleton

object SimpleByteCode extends TransformationGroup {
  override def dependencies: Set[Contract] = Set(InferredStackFrames, InferredMaxStack)

  override def dependants: Set[Contract] = Set(ExpressionSkeleton)
}
