package application.graphing.model.simplifications

import core.transformation.Contract
import transformations.javac._
import transformations.javac.classes.{ClassOrPackageReference, ClassOrPackageSelector}
import transformations.javac.constructor.{ImplicitSuperConstructorCall, DefaultConstructorC}

object JavaC extends TransformationGroup {
  override def dependencies: Set[Contract] =
    Set(ImplicitSuperConstructorCall, DefaultConstructorC, ImplicitObjectSuperClass, ClassOrPackageReference, ClassOrPackageSelector)

  override def dependants: Set[Contract] = Set.empty
}
