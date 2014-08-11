package application.graphing.model.simplifications

import core.transformation.Contract
import transformations.javac.expressions._
import transformations.javac.statements.StatementC

object JavaSimpleExpression extends TransformationGroup {

  override def dependencies: Set[Contract] =
    Set(LessThanC, AdditionC, LiteralC, SubtractionC, TernaryC, ParenthesisC, NullC, EqualityC)

  override def dependants: Set[Contract] = Set(StatementC)
}
