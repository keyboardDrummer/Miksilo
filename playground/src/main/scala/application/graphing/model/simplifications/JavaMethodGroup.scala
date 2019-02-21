package application.graphing.model.simplifications

import core.deltas.Contract
import deltas.expression.PostFixIncrementDelta
import deltas.javac.ImplicitObjectSuperClass
import deltas.javac.classes.{BasicImportDelta, FieldDeclarationDelta}
import deltas.javac.methods.{ImplicitReturnAtEndOfMethod, MemberSelectorDelta}
import deltas.statement.LocalDeclarationWithInitializerDelta
import deltas.statement.assignment.{AddAssignmentDelta, AssignToVariable}

object JavaMethodGroup extends DeltaGroup {

  override def dependencies: Set[Contract] = Set(ImplicitReturnAtEndOfMethod, LocalDeclarationWithInitializerDelta, AddAssignmentDelta, PostFixIncrementDelta, AssignToVariable)

  override def dependants: Set[Contract] = Set(ImplicitObjectSuperClass, MemberSelectorDelta, BasicImportDelta, FieldDeclarationDelta)
}
