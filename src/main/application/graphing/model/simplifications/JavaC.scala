package application.graphing.model.simplifications

import core.transformation.Contract
import transformations.javac._

object JavaC extends TransformationGroup {
  override def dependencies: Set[Contract] =
    Set(ImplicitSuperConstructorCall, DefaultConstructor, ImplicitObjectSuperClass)

  override def dependants: Set[Contract] = Set.empty
}
