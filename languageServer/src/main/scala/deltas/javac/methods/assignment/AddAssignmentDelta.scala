package deltas.javac.methods.assignment

import core.deltas._
import core.language.node.NodeShape
import deltas.expression.additive.AdditionDelta

object AddAssignmentDelta extends OperatorWithAssignmentDelta {

  object Shape extends NodeShape

  override def description: String = "Defines the += operator."

  override def dependencies: Set[Contract] = Set(AdditionDelta) ++ super.dependencies

  override def keyword = "+="

  override def operatorShape = AdditionDelta.Shape

  override val shape = Shape
}

