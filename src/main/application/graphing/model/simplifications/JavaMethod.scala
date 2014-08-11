package application.graphing.model.simplifications

import core.transformation.Contract
import transformations.javac.methods.{ImplicitReturnAtEndOfMethod, ReturnExpressionC}
import transformations.javac.statements.AssignmentC
import transformations.javac.{ConstructorC, ImplicitObjectSuperClass, ImplicitThisInPrivateCalls}

object JavaMethod extends TransformationGroup {

  override def dependencies: Set[Contract] = Set(ImplicitReturnAtEndOfMethod, ImplicitThisInPrivateCalls, AssignmentC, ReturnExpressionC)

  override def dependants: Set[Contract] = Set(ConstructorC, ImplicitObjectSuperClass)
}
