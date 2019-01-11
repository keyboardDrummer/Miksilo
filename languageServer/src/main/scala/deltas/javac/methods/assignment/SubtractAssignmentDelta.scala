package deltas.javac.methods.assignment

import core.deltas.Contract
import core.language.node.NodeShape
import deltas.expression.additive.{AdditionDelta, SubtractionDelta}

object SubtractAssignmentDelta extends OperatorWithAssignmentDelta {

  override def description: String = "Defines the -= operator."

  override def dependencies: Set[Contract] = Set(AdditionDelta) ++ super.dependencies

  override def keyword = "-="

  override def operatorShape = SubtractionDelta.Shape

  object Shape extends NodeShape
  override val shape = Shape
}
