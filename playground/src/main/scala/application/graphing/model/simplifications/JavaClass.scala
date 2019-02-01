package application.graphing.model.simplifications

import core.deltas.Contract
import deltas.javac.{ImplicitJavaLangImport, ImplicitObjectSuperClass}

object JavaClass extends DeltaGroup {

  override def dependencies: Set[Contract] = Set(JavaMethodGroup, ImplicitObjectSuperClass, ImplicitJavaLangImport)

  override def dependants: Set[Contract] = Set(JavaGroup)
}
