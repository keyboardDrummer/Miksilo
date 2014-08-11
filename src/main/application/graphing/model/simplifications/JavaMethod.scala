package application.graphing.model.simplifications

import core.transformation.Contract
import transformations.javac.statements.AssignmentC
import transformations.javac.{ConstructorC, ImplicitObjectSuperClass, ImplicitReturnAtEndOfMethod, ImplicitThisInPrivateCalls}

object JavaMethod extends TransformationGroup {

  override def dependencies: Set[Contract] = Set(ImplicitReturnAtEndOfMethod, ImplicitThisInPrivateCalls, AssignmentC)

  override def dependants: Set[Contract] = Set(ConstructorC, ImplicitObjectSuperClass)
}
