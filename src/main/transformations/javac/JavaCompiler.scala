package transformations.javac

import core.transformation._
import core.transformation.sillyCodePieces.Injector
import transformations.bytecode.instructions._
import transformations.bytecode.{InferredMaxStack, InferredStackFrames, LabelledJumps}
import transformations.javac.base.JavaMethodC
import transformations.javac.expressions._
import transformations.javac.methods._
import transformations.javac.statements.{AssignmentC, BlockC, DeclarationC, StatementC}
import transformations.javac.types.{ObjectTypeC, TypeC}

object JavaCompiler {
  def getCompiler = new CompilerFromTransformations(javaCompilerTransformations)

  def javaCompilerTransformations: Seq[Injector] = {
    Seq(ImplicitThisInPrivateCalls, ImplicitJavaLangImport, DefaultConstructor, ImplicitSuperConstructorCall,
      ImplicitObjectSuperClass, ImplicitReturnAtEndOfMethod, ConstructorC, LessThanC, TernaryC, EqualityC,
      AddEqualityPrecedence, AddRelationalPrecedence, AdditionC, SubtractionC, AddAdditivePrecedence, LiteralC,
      AssignmentC, CallC, ReturnC, SelectorC, VariableC, ParenthesisC, NullC, DeclarationC, JavaMethodC, BlockC,
      StatementC, ExpressionC, ObjectTypeC, TypeC, InferredStackFrames, InferredMaxStack, LabelledJumps) ++
      byteCodeTransformations
  }

  def byteCodeTransformations = Seq(AddIntegersC, GetStaticC, GotoC, IfIntegerCompareGreaterC,
    IfZeroC, IncrementIntegerC, IntegerConstantC, IntegerReturnC, InvokeSpecialC, InvokeVirtualC, InvokeStaticC,
    LoadAddressC, LoadIntegerC, PushNullC, StoreAddressC, StoreIntegerC, SubtractIntegerC, VoidReturnC)

  def getTransformer = new Transformer(javaCompilerTransformations)
}

object JavaExpression extends Contract {

  override def dependencies: Set[Contract] =
    Set(LessThanC, AdditionC, LiteralC, SubtractionC, TernaryC)
}

object JavaMinus extends Contract {
  override def dependencies: Set[Contract] =
    Set(JavaExpression, ImplicitThisInPrivateCalls, ImplicitJavaLangImport, ImplicitSuperConstructorCall, ImplicitObjectSuperClass, ImplicitReturnAtEndOfMethod, DefaultConstructor)
}