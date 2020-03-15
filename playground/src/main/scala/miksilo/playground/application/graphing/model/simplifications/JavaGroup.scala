package playground.application.graphing.model.simplifications

import core.deltas.Contract
import miksilo.modularLanguages.deltas.javac._
import miksilo.modularLanguages.deltas.javac.constructor.{DefaultConstructorDelta, ImplicitSuperConstructorCall}

object JavaGroup extends DeltaGroup {
  override def dependencies: Set[Contract] =
    Set(ImplicitSuperConstructorCall, DefaultConstructorDelta, ImplicitObjectSuperClass,
    ImplicitThisForPrivateMemberSelectionDelta, ImplicitJavaLangImport)

  override def dependants: Set[Contract] = Set.empty
}
