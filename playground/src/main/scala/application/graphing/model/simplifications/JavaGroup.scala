package application.graphing.model.simplifications

import core.deltas.Contract
import deltas.javac._
import deltas.javac.constructor.{DefaultConstructorDelta, ImplicitSuperConstructorCall}

object JavaGroup extends DeltaGroup {
  override def dependencies: Set[Contract] =
    Set(ImplicitSuperConstructorCall, DefaultConstructorDelta, ImplicitObjectSuperClass,
    ImplicitThisForPrivateMemberSelectionDelta, ImplicitJavaLangImport)

  override def dependants: Set[Contract] = Set.empty
}
