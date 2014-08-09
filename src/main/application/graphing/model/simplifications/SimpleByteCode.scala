package application.graphing.model.simplifications

import core.transformation.Contract
import transformations.bytecode.simpleBytecode.{InferredMaxStack, InferredStackFrames}
import transformations.javac.expressions.ExpressionC

object SimpleByteCode extends Simplification {
  override def dependencies: Set[Contract] = Set(InferredStackFrames, InferredMaxStack)

  override def dependants: Set[Contract] = Set(ExpressionC)
}
