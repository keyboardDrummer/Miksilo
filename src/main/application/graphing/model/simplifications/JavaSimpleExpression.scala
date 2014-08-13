package application.graphing.model.simplifications

import core.transformation.Contract
import transformations.javac.expressions._
import transformations.javac.expressions.additive.{AdditionC, SubtractionC}
import transformations.javac.expressions.equality.EqualityC
import transformations.javac.expressions.literals.{NumberLiteralC, NullC, BooleanLiteralC}
import transformations.javac.expressions.relational.LessThanC
import transformations.javac.statements.StatementC

object JavaSimpleExpression extends TransformationGroup {

  override def dependencies: Set[Contract] =
    Set(LessThanC, AdditionC, BooleanLiteralC, NumberLiteralC, SubtractionC, TernaryC, ParenthesisC, NullC, EqualityC)

  override def dependants: Set[Contract] = Set(StatementC)
}
