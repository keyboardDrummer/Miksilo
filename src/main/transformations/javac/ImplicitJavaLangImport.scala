package transformations.javac

import core.transformation.sillyCodePieces.ProgramTransformation
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.javac.base.MethodAndClassC
import transformations.javac.base.model.{JavaClassModel, JavaImport}

object ImplicitJavaLangImport extends ProgramTransformation {
  override def dependencies: Set[Contract] = Set(MethodAndClassC)

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    val clazz = program
    val imports = JavaClassModel.getImports(clazz)
    val implicitImport = new JavaImport(Seq(JavaLang.javaPackageName, JavaLang.langPackageName), None)
    clazz(JavaClassModel.ClassImports) = implicitImport :: imports
  }
}
