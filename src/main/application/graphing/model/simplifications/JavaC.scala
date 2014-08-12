package application.graphing.model.simplifications

import core.transformation.Contract
import transformations.javac._
import transformations.javac.classes.{ClassOrPackageReference, ClassOrPackageSelector}

object JavaC extends TransformationGroup {
  override def dependencies: Set[Contract] =
    Set(ImplicitSuperConstructorCall, DefaultConstructor, ImplicitObjectSuperClass, ClassOrPackageReference, ClassOrPackageSelector)

  override def dependants: Set[Contract] = Set.empty
}
