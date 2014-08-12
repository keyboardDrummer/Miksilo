package application.graphing.model.simplifications

import core.transformation.Contract
import transformations.javac.classes.ClassC
import transformations.javac.methods.{CallC, ImplicitReturnAtEndOfMethod, ReturnExpressionC, VariableC}
import transformations.javac.statements.AssignmentC

object JavaMethod extends TransformationGroup {

  override def dependencies: Set[Contract] = Set(ImplicitReturnAtEndOfMethod, AssignmentC, ReturnExpressionC, CallC, VariableC)

  override def dependants: Set[Contract] = Set(ClassC)
}
