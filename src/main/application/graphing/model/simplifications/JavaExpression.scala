package application.graphing.model.simplifications

import core.transformation.Contract
import transformations.javac.ImplicitThisInPrivateCalls
import transformations.javac.methods.{CallC, VariableC}
import transformations.javac.statements.AssignmentC

object JavaExpression extends Simplification {

  override def dependencies: Set[Contract] =
    Set(JavaSimpleExpression, AssignmentC, CallC, VariableC, ImplicitThisInPrivateCalls)

  override def dependants: Set[Contract] = Set()
}
