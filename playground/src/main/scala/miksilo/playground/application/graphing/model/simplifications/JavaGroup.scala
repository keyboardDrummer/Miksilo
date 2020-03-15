package miksilo.playground.application.graphing.model.simplifications

import miksilo.modularLanguages.core.deltas.Contract
import miksilo.modularLanguages.deltas.classes.constructor.{DefaultConstructorDelta, ImplicitSuperConstructorCall}
import miksilo.modularLanguages.deltas.javac._

object JavaGroup extends DeltaGroup {
  override def dependencies: Set[Contract] =
    Set(ImplicitSuperConstructorCall, DefaultConstructorDelta, ImplicitObjectSuperClass,
    ImplicitThisForPrivateMemberSelectionDelta, ImplicitJavaLangImport)

  override def dependants: Set[Contract] = Set.empty
}
