package languages.javac

import transformation.{TransformationState, MetaObject, ProgramTransformation}
import languages.javac.base.JavaBase
import languages.javac.base.model.{JavaClassModel, QualifiedClassName}

object ImplicitObjectSuperClass extends ProgramTransformation {
  val objectName = "Object"
  val packageName = Seq("java","lang")
  val qualifiedObjectName = new QualifiedClassName(packageName ++ Seq(objectName))
  override def dependencies: Set[ProgramTransformation] = Set(JavaBase)

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    if (JavaClassModel.getParent(program).isEmpty)
    {
      program(JavaClassModel.ClassParent) = objectName
    }
  }
}
