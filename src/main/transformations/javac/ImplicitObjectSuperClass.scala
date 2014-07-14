package transformations.javac

import core.transformation.{Contract, MetaObject, ProgramTransformation, TransformationState}
import transformations.javac.base.JavaMethodC
import transformations.javac.base.model.{JavaClassModel, QualifiedClassName}

object ImplicitObjectSuperClass extends ProgramTransformation {
  val objectName = "Object"
  val packageName = Seq("java", "lang")
  val qualifiedObjectName = new QualifiedClassName(packageName ++ Seq(objectName))

  override def dependencies: Set[Contract] = Set(JavaMethodC)

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    if (JavaClassModel.getParent(program).isEmpty) {
      program(JavaClassModel.ClassParent) = objectName
    }
  }
}
