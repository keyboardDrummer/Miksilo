package application.graphing.model.simplifications

import core.particles.Contract
import transformations.javac._
import transformations.javac.classes.{SelectorReferenceKind, VariableReferenceKind}
import transformations.javac.constructor.{DefaultConstructorC, ImplicitSuperConstructorCall}

object JavaC extends TransformationGroup {
  override def dependencies: Set[Contract] =
    Set(ImplicitSuperConstructorCall, DefaultConstructorC, ImplicitObjectSuperClass, VariableReferenceKind, SelectorReferenceKind,
    ImplicitThisForMemberSelectors, ImplicitJavaLangImport)

  override def dependants: Set[Contract] = Set.empty
}
