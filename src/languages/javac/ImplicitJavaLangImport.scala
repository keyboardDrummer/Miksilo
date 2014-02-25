package languages.javac

import transformation.{TransformationState, MetaObject, ProgramTransformation}
import languages.javac.base.{JavaImport, JavaClassModel, JavaBase}

object ImplicitJavaLangImport extends ProgramTransformation {
  override def dependencies: Set[ProgramTransformation] = Set(JavaBase)

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    val clazz = program
    val imports = JavaClassModel.getImports(clazz)
    val implicitImport = new JavaImport(Seq(JavaLang.javaPackageName, JavaLang.langPackageName), None)
    clazz(JavaClassModel.ClassImports) = implicitImport :: imports
  }
}
