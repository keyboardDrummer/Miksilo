package transformations.javac

import core.transformation._
import core.transformation.sillyCodePieces.Particle
import transformations.bytecode._
import transformations.bytecode.attributes.{CodeAttribute, LineNumberTable, StackMapTableAttribute}
import transformations.bytecode.coreInstructions._
import transformations.bytecode.coreInstructions.integers._
import transformations.bytecode.coreInstructions.integers.integerCompare._
import transformations.bytecode.coreInstructions.longs.{CompareLongC, LoadLongC, LongConstantC, StoreLongC}
import transformations.bytecode.coreInstructions.objects.{LoadAddressC, PushNullC, StoreAddressC}
import transformations.bytecode.extraBooleanInstructions._
import transformations.bytecode.additions.{LabelledTargets, PoptimizeC}
import transformations.bytecode.simpleBytecode.{InferredMaxStack, InferredStackFrames}
import transformations.javaPlus.ExpressionMethodC
import transformations.javac.classes._
import transformations.javac.constructor.{ConstructorC, DefaultConstructorC, ImplicitSuperConstructorCall}
import transformations.javac.expressions._
import transformations.javac.expressions.additive.{AddAdditivePrecedence, AdditionC, SubtractionC}
import transformations.javac.expressions.equality.{AddEqualityPrecedence, EqualityC}
import transformations.javac.expressions.literals.{BooleanLiteralC, IntLiteralC, LongLiteralC, NullC}
import transformations.javac.expressions.postfix.PostFixIncrementC
import transformations.javac.expressions.prefix.NotC
import transformations.javac.expressions.relational.{AddRelationalPrecedence, LessThanC}
import transformations.javac.methods._
import transformations.javac.methods.assignment.{AssignToVariable, AssignmentC, AssignmentPrecedence, IncrementAssignmentC}
import transformations.javac.statements._
import transformations.types._

object JavaCompiler {

  def getCompiler = new CompilerFromParticles(javaCompilerTransformations)

  def allTransformations = javaCompilerTransformations ++ Seq(ExpressionMethodC)

  def javaCompilerTransformations: Seq[Particle] = {
    Seq(ImplicitJavaLangImport, DefaultConstructorC, ImplicitSuperConstructorCall,
      ImplicitObjectSuperClass, ConstructorC, ClassOrPackageSelector, ClassOrPackageReference, ImplicitThisInPrivateCalls) ++ javaMethod
  }

  def javaMethod = Seq(ImplicitReturnAtEndOfMethod, IncrementAssignmentC,
    ReturnExpressionC, ReturnVoidC, CallC, SelectorC, DeclarationWithInitializerC, AssignToVariable, AssignmentC, AssignmentPrecedence, DeclarationC,
    PostFixIncrementC, VariableC, WildcardImportC, BasicImportC, ClassC, MethodC) ++
    javaSimpleStatement //todo move class.

  def javaSimpleStatement = Seq(IfThenC, ForLoopC, WhileC, BlockC,
    ExpressionAsStatementC, StatementC) ++ javaSimpleExpression

  def javaSimpleExpression = Seq(TernaryC, EqualityC,
    AddEqualityPrecedence, LessThanC, AddRelationalPrecedence, AdditionC, SubtractionC, AddAdditivePrecedence,
    BooleanLiteralC, LongLiteralC, IntLiteralC, NullC, NotC, ParenthesisC, ExpressionC) ++ allByteCodeTransformations

  def allByteCodeTransformations = Seq(OptimizeBooleanInstructionsC) ++
    Seq(LessThanInstructionC, NotInstructionC, IntegerEqualsInstructionC, ExpandInstructionsC) ++
    simpleByteCodeTransformations

  def simpleByteCodeTransformations = Seq(PoptimizeC) ++ Seq(InferredStackFrames, InferredMaxStack, LabelledTargets) ++ byteCodeTransformations

  def byteCodeTransformations = byteCodeInstructions ++ byteCodeWithoutInstructions

  def byteCodeWithoutInstructions = Seq(StackMapTableAttribute, LineNumberTable, CodeAttribute, ByteCodeSkeleton) ++ typeTransformations

  val typeTransformations = Seq(ObjectTypeC, ArrayTypeC, BooleanTypeC, DoubleTypeC, LongTypeC, VoidTypeC, IntTypeC, TypeC)

  def byteCodeInstructions: Seq[InstructionC] = {
    Seq(PopC, GetStaticC, GotoC, IfIntegerCompareLessC,
      IfZeroC, IfNotZero, InvokeSpecialC, InvokeVirtualC, InvokeStaticC,
      LoadAddressC, PushNullC, StoreAddressC, StoreIntegerC, SubtractIntegerC, VoidReturnInstructionC) ++
      integerInstructions ++ longInstructions
  }

  def longInstructions = Seq(CompareLongC, LongConstantC, LoadLongC, StoreLongC)

  def integerInstructions = Seq(AddIntegersC, IntegerConstantC, IncrementIntegerC, IntegerReturnInstructionC, LoadIntegerC, IfIntegerCompareGreaterOrEqualC,
    IfIntegerCompareEqualC, IfIntegerCompareNotEqualC)

  def spliceBeforeTransformations(implicits: Seq[Particle], splice: Seq[Particle]): Seq[Particle] = {
    val implicitsSet = implicits.toSet
    javaCompilerTransformations.filter(t => !implicitsSet.contains(t)) ++ splice ++ implicits
  }

  def spliceAfterTransformations(implicits: Seq[Particle], splice: Seq[Particle]): Seq[Particle] = {
    val implicitsSet = implicits.toSet
    implicits ++ splice ++ javaCompilerTransformations.filter(t => !implicitsSet.contains(t))
  }
}



