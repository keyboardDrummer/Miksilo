package application.graphing.model.simplifications

import core.transformation.Contract
import transformations.javac.expressions._

object JavaSimpleExpression extends Simplification {

  override def dependencies: Set[Contract] =
    Set(LessThanC, AdditionC, LiteralC, SubtractionC, TernaryC)

  override def dependants: Set[Contract] = Set()
}
