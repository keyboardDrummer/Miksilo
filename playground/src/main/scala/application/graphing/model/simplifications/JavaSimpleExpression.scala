package application.graphing.model.simplifications

import core.deltas.Contract
import deltas.expression.{IntLiteralDelta, ParenthesisInExpressionDelta}
import deltas.expression.additive.{AdditionDelta, SubtractionDelta}
import deltas.expression.relational.LessThanDelta
import deltas.javac.expressions._
import deltas.javac.expressions.equality.EqualityDelta
import deltas.javac.expressions.literals.{BooleanLiteralDelta, NullDelta}
import deltas.statement.StatementDelta
import deltas.statement.assignment.AssignmentPrecedence

object JavaSimpleExpression extends DeltaGroup {

  override def dependencies: Set[Contract] =
    Set(LessThanDelta, AdditionDelta, BooleanLiteralDelta, IntLiteralDelta, SubtractionDelta, TernaryDelta, ParenthesisInExpressionDelta, NullDelta, EqualityDelta) ++
      Set[Contract](AssignmentPrecedence) //TODO not sure

  override def dependants: Set[Contract] = Set(StatementDelta)
}
