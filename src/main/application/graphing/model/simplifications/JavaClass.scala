package application.graphing.model.simplifications

import core.transformation.Contract
import transformations.javac.{ImplicitJavaLangImport, ImplicitObjectSuperClass}

object JavaClass extends Simplification {

  override def dependencies: Set[Contract] = Set(JavaMethod, ImplicitObjectSuperClass, ImplicitJavaLangImport)

  override def dependants: Set[Contract] = Set(JavaC)
}
