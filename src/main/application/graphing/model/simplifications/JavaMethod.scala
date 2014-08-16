package application.graphing.model.simplifications

import core.transformation.Contract
import transformations.javac.classes.ClassC
import transformations.javac.methods.assignment.IncrementAssignmentC
import transformations.javac.methods.{CallC, ImplicitReturnAtEndOfMethod, VariableC}
import transformations.javac.statements.DeclarationWithInitializerC

object JavaMethod extends TransformationGroup {

  override def dependencies: Set[Contract] = Set(ImplicitReturnAtEndOfMethod, DeclarationWithInitializerC, CallC, VariableC, IncrementAssignmentC)

  override def dependants: Set[Contract] = Set(ClassC)
}
