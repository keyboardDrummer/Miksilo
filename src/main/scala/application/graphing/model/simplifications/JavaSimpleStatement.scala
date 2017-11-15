package application.graphing.model.simplifications

import core.particles.Contract
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.methods.assignment.AssignmentPrecedence
import deltas.javac.statements._
import deltas.javac.statements.locals.LocalDeclarationC

object JavaSimpleStatement extends TransformationGroup {

  override def dependencies: Set[Contract] = Set(LocalDeclarationC, IfThenC, ExpressionAsStatementC, ForLoopC)

  override def dependants: Set[Contract] = Set(JavaClassSkeleton)
}
