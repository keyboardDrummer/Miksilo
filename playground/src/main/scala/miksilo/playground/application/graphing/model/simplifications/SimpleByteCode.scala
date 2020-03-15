package playground.application.graphing.model.simplifications

import core.deltas.Contract
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.{InferredMaxStack, InferredStackFrames}
import miksilo.modularLanguages.deltas.expression.ExpressionDelta

object SimpleByteCode extends DeltaGroup {
  override def dependencies: Set[Contract] = Set(InferredStackFrames, InferredMaxStack)

  override def dependants: Set[Contract] = Set(ExpressionDelta)
}
