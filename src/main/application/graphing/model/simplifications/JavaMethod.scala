package application.graphing.model.simplifications

import core.transformation.Contract
import transformations.javac.classes.ClassC
import transformations.javac.expressions.postfix.PostFixIncrementC
import transformations.javac.methods.ImplicitReturnAtEndOfMethod
import transformations.javac.methods.assignment.{AssignToVariable, IncrementAssignmentC}
import transformations.javac.statements.DeclarationWithInitializerC

object JavaMethod extends TransformationGroup {

  override def dependencies: Set[Contract] = Set(ImplicitReturnAtEndOfMethod, DeclarationWithInitializerC, IncrementAssignmentC, PostFixIncrementC, AssignToVariable)

  override def dependants: Set[Contract] = Set(ClassC)
}
