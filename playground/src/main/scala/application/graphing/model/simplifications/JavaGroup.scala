package application.graphing.model.simplifications

import core.deltas.Contract
import deltas.javac._
import deltas.javac.classes.{MemberSelectorAsNamespaceReference, VariableAsNamespaceReference}
import deltas.javac.constructor.{DefaultConstructorDelta, ImplicitSuperConstructorCall}

object JavaGroup extends DeltaGroup {
  override def dependencies: Set[Contract] =
    Set(ImplicitSuperConstructorCall, DefaultConstructorDelta, ImplicitObjectSuperClass, VariableAsNamespaceReference, MemberSelectorAsNamespaceReference,
    ImplicitThisForPrivateMemberSelectionDelta, ImplicitJavaLangImport)

  override def dependants: Set[Contract] = Set.empty
}
