package transformations.javac

import core.transformation.{TransformationState, MetaObject, ProgramTransformation, TransformationManager}
import transformations.javac.base.JavaBase
import transformations.javac.TernaryC
import transformations.bytecode.{InferredStackFrames, InferredMaxStack, LabelledJumps}

object JavaCompiler {
  def getCompiler = TransformationManager.buildCompiler(javaCompilerTransformations)

  def javaCompilerTransformations: Seq[ProgramTransformation] = {
    Seq(ImplicitThisInPrivateCalls, ImplicitJavaLangImport, DefaultConstructor, ImplicitSuperConstructorCall, ImplicitObjectSuperClass, ImplicitReturnAtEndOfMethod, ConstructorC, LessThanC,
      AdditionC, LiteralC, SubtractionC, TernaryC, JavaBase, InferredStackFrames, InferredMaxStack, LabelledJumps)
  }
}

object JavaMinus extends ProgramTransformation {
  override def dependencies: Set[ProgramTransformation] =
    Set(ImplicitThisInPrivateCalls, ImplicitJavaLangImport, ImplicitSuperConstructorCall, ImplicitObjectSuperClass, ImplicitReturnAtEndOfMethod, DefaultConstructor, LessThanC,
      AdditionC, LiteralC, SubtractionC, TernaryC)

  override def transform(program: MetaObject, state: TransformationState): Unit = {

  }
}
