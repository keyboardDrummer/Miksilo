package transformations.javac

import core.transformation.{MetaObject, ProgramTransformation, TransformationManager, TransformationState}
import transformations.bytecode.{InferredMaxStack, InferredStackFrames, LabelledJumps}
import transformations.javac.base.JavaBase
import transformations.javac.expressions._

object JavaCompiler {
  def getCompiler = TransformationManager.buildCompiler(javaCompilerTransformations)

  def javaCompilerTransformations: Seq[ProgramTransformation] = {
    Seq(ImplicitThisInPrivateCalls, ImplicitJavaLangImport, DefaultConstructor, ImplicitSuperConstructorCall,
      ImplicitObjectSuperClass, ImplicitReturnAtEndOfMethod, ConstructorC, TernaryC, LessThanC, AddRelationalPrecedence,
      AdditionC, SubtractionC, AddAdditivePrecedence, LiteralC, JavaBase, InferredStackFrames, InferredMaxStack, LabelledJumps)
  }
}

object JavaMinus extends ProgramTransformation {
  override def dependencies: Set[ProgramTransformation] =
    Set(ImplicitThisInPrivateCalls, ImplicitJavaLangImport, ImplicitSuperConstructorCall, ImplicitObjectSuperClass, ImplicitReturnAtEndOfMethod, DefaultConstructor, LessThanC,
      AdditionC, LiteralC, SubtractionC, TernaryC)

  override def transform(program: MetaObject, state: TransformationState): Unit = {

  }
}