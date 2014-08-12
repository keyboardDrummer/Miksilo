package transformations.javac

import core.transformation.sillyCodePieces.ProgramTransformation
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.javac.classes.{ClassC, JavaImport}

object ImplicitJavaLangImport extends ProgramTransformation {
  override def dependencies: Set[Contract] = Set(ClassC)

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    val clazz = program
    val imports = ClassC.getImports(clazz)
    val implicitImport = new JavaImport(Seq(JavaLang.javaPackageName, JavaLang.langPackageName), None)
    clazz(ClassC.ClassImports) = implicitImport :: imports
  }
}
