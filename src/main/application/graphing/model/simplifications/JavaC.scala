package application.graphing.model.simplifications

import core.particles.Contract
import transformations.javac._
import transformations.javac.classes.{GetIdentifierKind, ClassOrPackageSelector}
import transformations.javac.constructor.{DefaultConstructorC, ImplicitSuperConstructorCall}

object JavaC extends TransformationGroup {
  override def dependencies: Set[Contract] =
    Set(ImplicitSuperConstructorCall, DefaultConstructorC, ImplicitObjectSuperClass, GetIdentifierKind, ClassOrPackageSelector,
    ImplicitThisInPrivateCalls, ImplicitJavaLangImport)

  override def dependants: Set[Contract] = Set.empty
}
