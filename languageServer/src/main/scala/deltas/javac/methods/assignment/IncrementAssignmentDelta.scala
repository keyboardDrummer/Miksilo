package deltas.javac.methods.assignment

import core.deltas._
import deltas.javac.expressions.additive.AdditionDelta

object IncrementAssignmentDelta extends OperatorWithAssignmentDelta {

  override def description: String = "Defines the += operator."

  override def dependencies: Set[Contract] = Set(AdditionDelta) ++ super.dependencies

  override def keyword = "+="

  override def operatorShape = AdditionDelta.Shape
}

