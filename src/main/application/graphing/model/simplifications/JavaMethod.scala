package application.graphing.model.simplifications

import core.transformation.Contract
import transformations.javac.ImplicitReturnAtEndOfMethod

object JavaMethod extends TransformationGroup {

  override def dependencies: Set[Contract] = Set(ImplicitReturnAtEndOfMethod, JavaStatement)

  override def dependants: Set[Contract] = Set()
}
