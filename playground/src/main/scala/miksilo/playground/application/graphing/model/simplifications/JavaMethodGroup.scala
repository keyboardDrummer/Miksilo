package playground.application.graphing.model.simplifications

import core.deltas.Contract
import miksilo.modularLanguages.deltas.expression.PostFixIncrementDelta
import miksilo.modularLanguages.deltas.javac.ImplicitObjectSuperClass
import miksilo.modularLanguages.deltas.javac.classes.{BasicImportDelta, FieldDeclarationDelta}
import miksilo.modularLanguages.deltas.javac.methods.{ImplicitReturnAtEndOfMethod, MemberSelectorDelta}
import miksilo.modularLanguages.deltas.statement.LocalDeclarationWithInitializerDelta
import miksilo.modularLanguages.deltas.statement.assignment.{AddAssignmentDelta, AssignToVariable}

object JavaMethodGroup extends DeltaGroup {

  override def dependencies: Set[Contract] = Set(ImplicitReturnAtEndOfMethod, LocalDeclarationWithInitializerDelta, AddAssignmentDelta, PostFixIncrementDelta, AssignToVariable)

  override def dependants: Set[Contract] = Set(ImplicitObjectSuperClass, MemberSelectorDelta, BasicImportDelta, FieldDeclarationDelta)
}
