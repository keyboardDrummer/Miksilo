package application.graphing.model.simplifications

import core.deltas.Contract
import deltas.javac._
import deltas.javac.classes.{SelectorReferenceKind, VariableReferenceKind}
import deltas.javac.constructor.{DefaultConstructorC, ImplicitSuperConstructorCall}

object JavaC extends TransformationGroup {
  override def dependencies: Set[Contract] =
    Set(ImplicitSuperConstructorCall, DefaultConstructorC, ImplicitObjectSuperClass, VariableReferenceKind, SelectorReferenceKind,
    ImplicitThisForPrivateMemberSelection, ImplicitJavaLangImport)

  override def dependants: Set[Contract] = Set.empty
}
