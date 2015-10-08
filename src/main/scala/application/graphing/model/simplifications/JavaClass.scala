package application.graphing.model.simplifications

import core.particles.Contract
import transformations.javac.{ImplicitJavaLangImport, ImplicitObjectSuperClass}

object JavaClass extends TransformationGroup {

  override def dependencies: Set[Contract] = Set(JavaMethod, ImplicitObjectSuperClass, ImplicitJavaLangImport)

  override def dependants: Set[Contract] = Set(JavaC)
}
