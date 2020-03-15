package playground.application.graphing.model.simplifications

import core.deltas.Contract
import deltas.expression.additive.{AdditionDelta, SubtractionDelta}
import deltas.expression.relational.{EqualsComparisonDelta, LessThanDelta}
import deltas.expression.{IntLiteralDelta, ParenthesisInExpressionDelta, TernaryDelta}
import deltas.javac.expressions.literals.{BooleanLiteralDelta, NullDelta}
import deltas.statement.StatementDelta
import deltas.statement.assignment.AssignmentPrecedence

object JavaSimpleExpression extends DeltaGroup {

  override def dependencies: Set[Contract] =
    Set(LessThanDelta, AdditionDelta, BooleanLiteralDelta, IntLiteralDelta, SubtractionDelta, TernaryDelta, ParenthesisInExpressionDelta, NullDelta, EqualsComparisonDelta) ++
      Set[Contract](AssignmentPrecedence) //TODO not sure

  override def dependants: Set[Contract] = Set(StatementDelta)
}
