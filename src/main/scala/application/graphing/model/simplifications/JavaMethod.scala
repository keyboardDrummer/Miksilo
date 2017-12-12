package application.graphing.model.simplifications

import core.deltas.Contract
import deltas.javac.ImplicitObjectSuperClass
import deltas.javac.classes.{BasicImportC, FieldDeclaration}
import deltas.javac.expressions.postfix.PostFixIncrementC
import deltas.javac.methods.assignment.{AssignToVariable, IncrementAssignmentDelta}
import deltas.javac.methods.{ImplicitReturnAtEndOfMethod, MemberSelector}
import deltas.javac.statements.locals.LocalDeclarationWithInitializerC

object JavaMethod extends DeltaGroup {

  override def dependencies: Set[Contract] = Set(ImplicitReturnAtEndOfMethod, LocalDeclarationWithInitializerC, IncrementAssignmentDelta, PostFixIncrementC, AssignToVariable)

  override def dependants: Set[Contract] = Set(ImplicitObjectSuperClass, MemberSelector, BasicImportC, FieldDeclaration)
}
