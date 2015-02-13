package transformations.javac

import core.transformation.sillyCodePieces.ParticleWithPhase
import core.transformation.{Contract, MetaObject, TransformationState}
import transformations.javac.classes.{ClassC, WildcardImportC}

object ImplicitJavaLangImport extends ParticleWithPhase {
  override def dependencies: Set[Contract] = Set(ClassC, WildcardImportC)

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    val clazz = program
    val imports = ClassC.getImports(clazz)
    val implicitImport = WildcardImportC.wildCardImport(Seq(JavaLang.javaPackageName, JavaLang.langPackageName))
    clazz(ClassC.ClassImports) = Seq(implicitImport) ++ imports
  }
}
