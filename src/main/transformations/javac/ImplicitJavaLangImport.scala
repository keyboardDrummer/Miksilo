package transformations.javac

import core.transformation.{MetaObject, ProgramTransformation, TransformationState}
import transformations.javac.base.JavaMethodC
import transformations.javac.base.model.{JavaClassModel, JavaImport}

object ImplicitJavaLangImport extends ProgramTransformation {
  override def dependencies: Set[ProgramTransformation] = Set(JavaMethodC)

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    val clazz = program
    val imports = JavaClassModel.getImports(clazz)
    val implicitImport = new JavaImport(Seq(JavaLang.javaPackageName, JavaLang.langPackageName), None)
    clazz(JavaClassModel.ClassImports) = implicitImport :: imports
  }
}
