package application.graphing.model.simplifications

import core.particles.Contract
import transformations.javac.ImplicitObjectSuperClass
import transformations.javac.classes.{BasicImportC, FieldDeclaration}
import transformations.javac.expressions.postfix.PostFixIncrementC
import transformations.javac.methods.assignment.{AssignToVariable, IncrementAssignmentC}
import transformations.javac.methods.{ImplicitReturnAtEndOfMethod, MemberSelector}
import transformations.javac.statements.locals.LocalDeclarationWithInitializerC

object JavaMethod extends TransformationGroup {

  override def dependencies: Set[Contract] = Set(ImplicitReturnAtEndOfMethod, LocalDeclarationWithInitializerC, IncrementAssignmentC, PostFixIncrementC, AssignToVariable)

  override def dependants: Set[Contract] = Set(ImplicitObjectSuperClass, MemberSelector, BasicImportC, FieldDeclaration)
}
