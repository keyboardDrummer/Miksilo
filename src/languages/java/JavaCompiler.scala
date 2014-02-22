package languages.java

import transformation.{TransformationState, MetaObject, ProgramTransformation, TransformationManager}
import languages.java.base.JavaBase
import languages.java.TernaryC
import languages.bytecode.ByteCodeGoTo

object JavaCompiler {
  def getCompiler = TransformationManager.buildCompiler(javaCompilerTransformations)

  def javaCompilerTransformations: Seq[ProgramTransformation] = {
    Seq(ImplicitJavaLangImport, ImplicitSuperConstructorCall, ImplicitObjectSuperClass, ImplicitReturnAtEndOfMethod, DefaultConstructor, ConstructorC, LessThanC,
      AdditionC, LiteralC, SubtractionC, TernaryC, JavaBase, ByteCodeGoTo)
  }
}

object JavaMinus extends ProgramTransformation {
  override def dependencies: Set[ProgramTransformation] =
    Set(ConstructorC, ImplicitJavaLangImport, ImplicitSuperConstructorCall, ImplicitObjectSuperClass, ImplicitReturnAtEndOfMethod, DefaultConstructor, LessThanC,
      AdditionC, LiteralC, SubtractionC, TernaryC, JavaBase)

  override def transform(program: MetaObject, state: TransformationState): Unit = {

  }
}
