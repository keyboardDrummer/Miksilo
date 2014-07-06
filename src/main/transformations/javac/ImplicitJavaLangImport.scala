package transformations.javac

import core.transformation.{TransformationState, MetaObject, ProgramTransformation}
import transformations.javac.base.JavaBase
import transformations.javac.base.model.{JavaClassModel, JavaImport}

object ImplicitJavaLangImport extends ProgramTransformation {
  override def dependencies: Set[ProgramTransformation] = Set(JavaBase)

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    val clazz = program
    val imports = JavaClassModel.getImports(clazz)
    val implicitImport = new JavaImport(Seq(JavaLang.javaPackageName, JavaLang.langPackageName), None)
    clazz(JavaClassModel.ClassImports) = implicitImport :: imports
  }
}
