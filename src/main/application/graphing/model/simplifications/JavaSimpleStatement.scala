package application.graphing.model.simplifications

import core.particles.Contract
import transformations.javac.methods.MethodC
import transformations.javac.methods.assignment.AssignmentPrecedence
import transformations.javac.statements._
import transformations.javac.statements.locals.LocalDeclarationC

object JavaSimpleStatement extends TransformationGroup {

  override def dependencies: Set[Contract] = Set(LocalDeclarationC, IfThenC, ExpressionAsStatementC, ForLoopC)

  override def dependants: Set[Contract] = Set(MethodC, AssignmentPrecedence)
}
