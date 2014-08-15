package transformations.javac

import core.transformation._
import core.transformation.sillyCodePieces.Injector
import transformations.bytecode.coreInstructions._
import transformations.bytecode.coreInstructions.integerCompare.{IfIntegerCompareGreaterOrEqualC, IfIntegerCompareLessC, IfZeroC}
import transformations.bytecode.extraBooleanInstructions.{LessThanInstructionC, OptimizeBooleanInstructionsC}
import transformations.bytecode.simpleBytecode.{InferredMaxStack, InferredStackFrames}
import transformations.bytecode.{PoptimizeC, ByteCodeSkeleton, LabelledTargets}
import transformations.javac.classes.{ClassC, ClassOrPackageReference, ClassOrPackageSelector}
import transformations.javac.constructor.{ConstructorC, DefaultConstructorC, ImplicitSuperConstructorCall}
import transformations.javac.expressions._
import transformations.javac.expressions.additive.{AddAdditivePrecedence, AdditionC, SubtractionC}
import transformations.javac.expressions.equality.{AddEqualityPrecedence, EqualityC}
import transformations.javac.expressions.literals.{BooleanLiteralC, NullC, NumberLiteralC}
import transformations.javac.expressions.postfix.PostFixIncrementC
import transformations.javac.expressions.relational.{AddRelationalPrecedence, LessThanC}
import transformations.javac.methods._
import transformations.javac.methods.assignment.{AssignmentC, AssignmentPrecedence, IncrementAssignmentC}
import transformations.javac.statements._
import transformations.types._

object JavaCompiler {
  val typeTransformations = Seq(ObjectTypeC, ArrayTypeC, BooleanTypeC, LongTypeC, VoidTypeC, IntTypeC, TypeC)

  def getCompiler = new CompilerFromTransformations(javaCompilerTransformations)

  def javaCompilerTransformations: Seq[Injector] = {
    Seq(ImplicitJavaLangImport, DefaultConstructorC, ImplicitSuperConstructorCall,
      ImplicitObjectSuperClass, ConstructorC, ClassOrPackageSelector, SelectorC, ClassOrPackageReference, ImplicitThisInPrivateCalls) ++ javaMethod
  }

  def javaMethod = Seq(ImplicitReturnAtEndOfMethod, IncrementAssignmentC, AssignmentC, AssignmentPrecedence,
    ReturnExpressionC, ReturnVoidC, CallC, DeclarationWithInitializerC, DeclarationC, VariableC, ClassC, MethodC) ++ javaSimpleStatement //todo move class.

  def javaSimpleStatement = Seq(ForLoopC, WhileC, BlockC,
    ExpressionAsStatementC, StatementC) ++ javaSimpleExpression

  def javaSimpleExpression = Seq(TernaryC, EqualityC,
    AddEqualityPrecedence, LessThanC, AddRelationalPrecedence, AdditionC, SubtractionC, AddAdditivePrecedence,
    PostFixIncrementC, BooleanLiteralC, NumberLiteralC, NullC, ParenthesisC, ExpressionC) ++ allByteCodeTransformations

  def allByteCodeTransformations = Seq(OptimizeBooleanInstructionsC) ++ Seq(LessThanInstructionC) ++ simpleByteCodeTransformations

  def simpleByteCodeTransformations = Seq(PoptimizeC) ++ Seq(InferredStackFrames, InferredMaxStack, LabelledTargets) ++ byteCodeTransformations

  def byteCodeTransformations = typeTransformations ++ byteCodeInstructions ++ Seq(ByteCodeSkeleton)

  def byteCodeInstructions: Seq[InstructionC] = {
    Seq(PopC, AddIntegersC, GetStaticC, GotoC, IfIntegerCompareLessC, IfIntegerCompareGreaterOrEqualC,
      IfZeroC, IncrementIntegerC, IntegerConstantC, IntegerReturnInstructionC, InvokeSpecialC, InvokeVirtualC, InvokeStaticC,
      LoadAddressC, LoadIntegerC, PushNullC, StoreAddressC, StoreIntegerC, SubtractIntegerC, VoidReturnInstructionC)
  }

  def getTransformer = new Transformer(javaCompilerTransformations)
}



