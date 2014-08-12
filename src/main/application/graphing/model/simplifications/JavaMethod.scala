package application.graphing.model.simplifications

import core.transformation.Contract
import transformations.javac.classes.ClassC
import transformations.javac.methods.assignment.AssignmentC
import transformations.javac.methods.{CallC, ImplicitReturnAtEndOfMethod, VariableC}

object JavaMethod extends TransformationGroup {

  override def dependencies: Set[Contract] = Set(ImplicitReturnAtEndOfMethod, AssignmentC, CallC, VariableC)

  override def dependants: Set[Contract] = Set(ClassC)
}
