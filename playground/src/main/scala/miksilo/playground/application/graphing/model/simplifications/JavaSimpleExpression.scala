package playground.application.graphing.model.simplifications

import core.deltas.Contract
import miksilo.modularLanguages.deltas.expression.additive.{AdditionDelta, SubtractionDelta}
import miksilo.modularLanguages.deltas.expression.relational.{EqualsComparisonDelta, LessThanDelta}
import miksilo.modularLanguages.deltas.expression.{IntLiteralDelta, ParenthesisInExpressionDelta, TernaryDelta}
import miksilo.modularLanguages.deltas.javac.expressions.literals.{BooleanLiteralDelta, NullDelta}
import miksilo.modularLanguages.deltas.statement.StatementDelta
import miksilo.modularLanguages.deltas.statement.assignment.AssignmentPrecedence

object JavaSimpleExpression extends DeltaGroup {

  override def dependencies: Set[Contract] =
    Set(LessThanDelta, AdditionDelta, BooleanLiteralDelta, IntLiteralDelta, SubtractionDelta, TernaryDelta, ParenthesisInExpressionDelta, NullDelta, EqualsComparisonDelta) ++
      Set[Contract](AssignmentPrecedence) //TODO not sure

  override def dependants: Set[Contract] = Set(StatementDelta)
}
