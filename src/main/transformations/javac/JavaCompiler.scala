package transformations.javac

import core.transformation.{MetaObject, ProgramTransformation, TransformationManager, TransformationState}
import transformations.bytecode.{InferredMaxStack, InferredStackFrames, LabelledJumps}
import transformations.javac.base.JavaMethodC
import transformations.javac.expressions._
import transformations.javac.methods._
import transformations.javac.statements.{BlockC, StatementC}

object JavaCompiler {
  def getCompiler = TransformationManager.buildCompiler(javaCompilerTransformations)

  def javaCompilerTransformations: Seq[ProgramTransformation] = {
    Seq(ImplicitThisInPrivateCalls, ImplicitJavaLangImport, DefaultConstructor, ImplicitSuperConstructorCall,
      ImplicitObjectSuperClass, ImplicitReturnAtEndOfMethod, ConstructorC, TernaryC, LessThanC, AddRelationalPrecedence,
      AdditionC, SubtractionC, AddAdditivePrecedence, LiteralC, CallC, ParenthesisC, ReturnC, SelectorC, VariableC,
      JavaMethodC, BlockC, StatementC, ExpressionC, InferredStackFrames, InferredMaxStack, LabelledJumps)
  }
}

object JavaExpression extends ProgramTransformation {

  override def dependencies: Set[ProgramTransformation] =
    Set(LessThanC, AdditionC, LiteralC, SubtractionC, TernaryC)

  override def transform(program: MetaObject, state: TransformationState): Unit = {

  }
}

object JavaMinus extends ProgramTransformation {
  override def dependencies: Set[ProgramTransformation] =
    Set(JavaExpression, ImplicitThisInPrivateCalls, ImplicitJavaLangImport, ImplicitSuperConstructorCall, ImplicitObjectSuperClass, ImplicitReturnAtEndOfMethod, DefaultConstructor)

  override def transform(program: MetaObject, state: TransformationState): Unit = {

  }
}