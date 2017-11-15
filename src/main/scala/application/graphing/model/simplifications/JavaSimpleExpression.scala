package application.graphing.model.simplifications

import core.particles.Contract
import deltas.javac.expressions._
import deltas.javac.expressions.additive.{AdditionDelta, SubtractionC}
import deltas.javac.expressions.equality.EqualityDelta
import deltas.javac.expressions.literals.{BooleanLiteralC, IntLiteralDelta, NullC}
import deltas.javac.expressions.relational.LessThanC
import deltas.javac.methods.assignment.AssignmentPrecedence
import deltas.javac.statements.StatementSkeleton

object JavaSimpleExpression extends TransformationGroup {

  override def dependencies: Set[Contract] =
    Set(LessThanC, AdditionDelta, BooleanLiteralC, IntLiteralDelta, SubtractionC, TernaryC, ParenthesisC, NullC, EqualityDelta) ++
      Set[Contract](AssignmentPrecedence) //TODO not sure

  override def dependants: Set[Contract] = Set(StatementSkeleton)
}
