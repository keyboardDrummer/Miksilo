package transformations.javac

import core.transformation._
import transformations.bytecode.{InferredMaxStack, InferredStackFrames, LabelledJumps}
import transformations.javac.base.JavaMethodC
import transformations.javac.expressions._
import transformations.javac.methods._
import transformations.javac.statements.{BlockC, StatementC}

object JavaCompiler {
  def getCompiler = TransformationManager.buildCompiler(javaCompilerTransformations)

  def javaCompilerTransformations: Seq[ProgramTransformation] = {
    Seq(ImplicitThisInPrivateCalls, ImplicitJavaLangImport, DefaultConstructor, ImplicitSuperConstructorCall,
      ImplicitObjectSuperClass, ImplicitReturnAtEndOfMethod, ConstructorC, LessThanC, AddRelationalPrecedence,
      AdditionC, SubtractionC, AddAdditivePrecedence, LiteralC, TernaryC, CallC, ReturnC, SelectorC, VariableC, ParenthesisC,
      JavaMethodC, BlockC, StatementC, ExpressionC, InferredStackFrames, InferredMaxStack, LabelledJumps)
  }
}

object JavaExpression extends Contract {

  override def dependencies: Set[Contract] =
    Set(LessThanC, AdditionC, LiteralC, SubtractionC, TernaryC)
}

object JavaMinus extends Contract {
  override def dependencies: Set[Contract] =
    Set(JavaExpression, ImplicitThisInPrivateCalls, ImplicitJavaLangImport, ImplicitSuperConstructorCall, ImplicitObjectSuperClass, ImplicitReturnAtEndOfMethod, DefaultConstructor)
}