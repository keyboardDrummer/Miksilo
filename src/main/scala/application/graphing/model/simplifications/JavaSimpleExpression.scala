package application.graphing.model.simplifications

import core.particles.Contract
import transformations.javac.expressions._
import transformations.javac.expressions.additive.{AdditionDelta, SubtractionC}
import transformations.javac.expressions.equality.EqualityDelta
import transformations.javac.expressions.literals.{BooleanLiteralC, IntLiteralDelta, NullC}
import transformations.javac.expressions.relational.LessThanC
import transformations.javac.methods.assignment.AssignmentPrecedence
import transformations.javac.statements.StatementSkeleton

object JavaSimpleExpression extends TransformationGroup {

  override def dependencies: Set[Contract] =
    Set(LessThanC, AdditionDelta, BooleanLiteralC, IntLiteralDelta, SubtractionC, TernaryC, ParenthesisC, NullC, EqualityDelta) ++
      Set[Contract](AssignmentPrecedence) //TODO not sure

  override def dependants: Set[Contract] = Set(StatementSkeleton)
}
