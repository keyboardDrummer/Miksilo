package languages.javac

import transformation.{TransformationState, MetaObject, ProgramTransformation, TransformationManager}
import languages.javac.base.JavaBase
import languages.javac.TernaryC
import languages.bytecode.ByteCodeGoTo

object JavaCompiler {
  def getCompiler = TransformationManager.buildCompiler(javaCompilerTransformations)

  def javaCompilerTransformations: Seq[ProgramTransformation] = {
    Seq(ImplicitThisInPrivateCalls, ImplicitJavaLangImport, DefaultConstructor, ImplicitSuperConstructorCall, ImplicitObjectSuperClass, ImplicitReturnAtEndOfMethod, ConstructorC, LessThanC,
      AdditionC, LiteralC, SubtractionC, TernaryC, JavaBase, ByteCodeGoTo)
  }
}

object JavaMinus extends ProgramTransformation {
  override def dependencies: Set[ProgramTransformation] =
    Set(ImplicitThisInPrivateCalls, ConstructorC, ImplicitJavaLangImport, ImplicitSuperConstructorCall, ImplicitObjectSuperClass, ImplicitReturnAtEndOfMethod, DefaultConstructor, LessThanC,
      AdditionC, LiteralC, SubtractionC, TernaryC, JavaBase)

  override def transform(program: MetaObject, state: TransformationState): Unit = {

  }
}
