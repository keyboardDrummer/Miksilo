package transformations.javac

import core.transformation._
import core.transformation.sillyCodePieces.Injector
import transformations.bytecode.coreInstructions._
import transformations.bytecode.coreInstructions.integerCompare.{IfIntegerCompareGreaterOrEqualC, IfIntegerCompareLessC, IfZeroC}
import transformations.bytecode.extraBooleanInstructions.{LessThanInstructionC, OptimizeBooleanInstructionsC}
import transformations.bytecode.simpleBytecode.{InferredMaxStack, InferredStackFrames}
import transformations.bytecode.{ByteCodeSkeleton, LabelledTargets}
import transformations.javac.classes.{ClassC, ClassOrPackageReference, ClassOrPackageSelector}
import transformations.javac.expressions._
import transformations.javac.expressions.additive.{AddAdditivePrecedence, AdditionC, SubtractionC}
import transformations.javac.expressions.equality.{AddEqualityPrecedence, EqualityC}
import transformations.javac.expressions.postfix.{PostFixPrecedenceC, PostfixPlusPlusC}
import transformations.javac.expressions.relational.{AddRelationalPrecedence, LessThanC}
import transformations.javac.methods._
import transformations.javac.statements._
import transformations.types._

object JavaCompiler {
  val typeTransformations = Seq(ObjectTypeC, ArrayTypeC, BooleanTypeC, LongTypeC, VoidTypeC, IntTypeC, TypeC)

  def getCompiler = new CompilerFromTransformations(javaCompilerTransformations)

  def javaCompilerTransformations: Seq[Injector] = {
    Seq(ImplicitThisInPrivateCalls, ImplicitJavaLangImport, DefaultConstructor, ImplicitSuperConstructorCall,
      ImplicitObjectSuperClass, ImplicitReturnAtEndOfMethod, ConstructorC, LessThanC, TernaryC, EqualityC,
      AddEqualityPrecedence, AddRelationalPrecedence, AdditionC, SubtractionC, AddAdditivePrecedence,
      PostfixPlusPlusC, PostFixPrecedenceC, BooleanLiteralC, NumberLiteralC,
      DeclarationWithInitializerC, AssignmentC, CallC, ReturnExpressionC, ReturnVoidC, ClassOrPackageSelector,
      SelectorC, ClassOrPackageReference, VariableC,
      ParenthesisC, NullC, DeclarationC, ClassC, MethodC, ForLoopC, WhileC, BlockC,
      StatementC, ExpressionC) ++ allByteCodeTransformations
  }

  def allByteCodeTransformations = Seq(OptimizeBooleanInstructionsC) ++ Seq(LessThanInstructionC) ++ simpleByteCodeTransformations

  def simpleByteCodeTransformations = Seq(InferredStackFrames, InferredMaxStack, LabelledTargets) ++ byteCodeTransformations

  def byteCodeTransformations = typeTransformations ++ byteCodeInstructions ++ Seq(ByteCodeSkeleton)

  def byteCodeInstructions: Seq[InstructionC] = {
    Seq(AddIntegersC, GetStaticC, GotoC, IfIntegerCompareLessC, IfIntegerCompareGreaterOrEqualC,
      IfZeroC, IncrementIntegerC, IntegerConstantC, IntegerReturnInstructionC, InvokeSpecialC, InvokeVirtualC, InvokeStaticC,
      LoadAddressC, LoadIntegerC, PushNullC, StoreAddressC, StoreIntegerC, SubtractIntegerC, VoidReturnInstructionC)
  }

  def getTransformer = new Transformer(javaCompilerTransformations)
}



