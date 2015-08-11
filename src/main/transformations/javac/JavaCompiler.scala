package transformations.javac

import application.compilerCockpit.PrettyPrint
import core.particles._
import transformations.bytecode._
import transformations.bytecode.additions.{LabelledTargets, PoptimizeC}
import transformations.bytecode.attributes._
import transformations.bytecode.constants._
import transformations.bytecode.coreInstructions._
import transformations.bytecode.coreInstructions.integers._
import transformations.bytecode.coreInstructions.integers.integerCompare._
import transformations.bytecode.coreInstructions.longs._
import transformations.bytecode.coreInstructions.objects._
import transformations.bytecode.extraBooleanInstructions._
import transformations.bytecode.simpleBytecode.{InferredMaxStack, InferredStackFrames}
import transformations.javaPlus.ExpressionMethodC
import transformations.javac.classes._
import transformations.javac.classes.skeleton.JavaClassSkeleton
import transformations.javac.constructor._
import transformations.javac.expressions._
import transformations.javac.expressions.additive.{AddAdditivePrecedence, AdditionC, SubtractionC}
import transformations.javac.expressions.equality.{AddEqualityPrecedence, EqualityC}
import transformations.javac.expressions.literals.{BooleanLiteralC, IntLiteralC, LongLiteralC, NullC}
import transformations.javac.expressions.postfix.PostFixIncrementC
import transformations.javac.expressions.prefix.NotC
import transformations.javac.expressions.relational.{AddRelationalPrecedence, LessThanC}
import transformations.javac.methods._
import transformations.javac.methods.assignment.{AssignToVariable, AssignmentPrecedence, AssignmentSkeleton, IncrementAssignmentC}
import transformations.javac.methods.call.CallStaticOrInstanceC
import transformations.javac.statements._
import transformations.javac.statements.locals.{LocalDeclarationC, LocalDeclarationWithInitializerC}
import transformations.bytecode.types._
import transformations.javac.types._

object JavaCompiler {

  def getCompiler = new CompilerFromParticles(javaCompilerTransformations)

  def allTransformations = javaCompilerTransformations ++ Seq(ExpressionMethodC, BlockCompilerC)

  def javaCompilerTransformations: Seq[Particle] = {
    Seq(ClassifyTypeIdentifiers, DefaultConstructorC, ImplicitSuperConstructorCall, ImplicitObjectSuperClass,
      NewC, FieldDeclarationWithInitializer, ConstructorC, SelectorReferenceKind, VariableReferenceKind) ++
      Seq(ThisCallExpression, SuperCallExpression, ThisVariable) ++ fields ++ imports ++
      javaMethod
  }

  def imports = Seq(ImplicitJavaLangImport, WildcardImportC, BasicImportC)
  def fields = Seq(FieldDeclaration, AssignToMember)

  def javaMethod = Seq(ForLoopC, LocalDeclarationWithInitializerC) ++
    Seq(ImplicitReturnAtEndOfMethod, ImplicitThisForPrivateMemberSelection, ReturnExpressionC, ReturnVoidC, CallStaticOrInstanceC, SelectField, MemberSelector) ++ methodBlock
  def methodBlock = Seq(LocalDeclarationC, IncrementAssignmentC, AssignToVariable, AssignmentSkeleton,
    AssignmentPrecedence, PostFixIncrementC, VariableC) ++ Seq(MethodC) ++ Seq(JavaClassSkeleton) ++ javaSimpleStatement

  def javaSimpleStatement = Seq(IfThenElseC, IfThenC, ContinueC, WhileC, BlockC,
    ExpressionAsStatementC, StatementSkeleton) ++ javaSimpleExpression

  def javaSimpleExpression: Seq[Particle] = Seq(TernaryC, EqualityC,
    AddEqualityPrecedence, LessThanC, AddRelationalPrecedence, AdditionC, SubtractionC, AddAdditivePrecedence,
    BooleanLiteralC, LongLiteralC, IntLiteralC, NullC, NotC, ParenthesisC, ExpressionSkeleton) ++ allByteCodeTransformations

  def allByteCodeTransformations = Seq(OptimizeBooleanInstructionsC) ++
    Seq(LessThanInstructionC, NotInstructionC, IntegerEqualsInstructionC, ExpandInstructionsC) ++
    simpleByteCodeTransformations

  def simpleByteCodeTransformations = Seq(PoptimizeC) ++ Seq(InferredStackFrames, InferredMaxStack, LabelledTargets) ++ byteCodeTransformations

  def byteCodeTransformations = byteCodeInstructions ++ byteCodeWithoutInstructions

  def byteCodeInstructions: Seq[InstructionC] = {
    Seq(Pop2C, PopC, GetStaticC, GotoC, IfIntegerCompareLessC,
      IfZeroC, IfNotZero, InvokeSpecialC, InvokeVirtualC, InvokeStaticC, NewByteCodeC, Duplicate2InstructionC, DuplicateInstructionC,
      LoadAddressC, PushNullC, StoreAddressC, StoreIntegerC, SubtractIntegerC, VoidReturnInstructionC,
      SwapInstruction, GetFieldC, PutField) ++
      integerInstructions ++ longInstructions
  }

  def longInstructions = Seq(CompareLongC, PushLongC, LoadLongC, StoreLongC)

  def integerInstructions = Seq(AddIntegersC, SmallIntegerConstantC, LoadConstantIntC, IncrementIntegerC, IntegerReturnInstructionC, LoadIntegerC, IfIntegerCompareGreaterOrEqualC,
    IfIntegerCompareEqualC, IfIntegerCompareNotEqualC)

  def byteCodeWithoutInstructions = byteCodeWithoutTextualParser ++ Seq(ParseUsingTextualGrammar)

  def byteCodeWithoutTextualParser: Seq[ParticleWithGrammar] = bytecodeAttributes ++ constantEntryParticles ++
    Seq(ByteCodeMethodInfo, ByteCodeFieldInfo) ++
    typeTransformations ++ Seq(ByteCodeSkeleton)

  val bytecodeAttributes: Seq[ParticleWithGrammar] = Seq(StackMapTableAttribute, LineNumberTable, SourceFileAttribute,
    CodeAttribute, //ExceptionsAttribute, InnerClassesAttribute,
    SignatureAttribute)

  def constantEntryParticles = Seq(DoubleConstantEntryC, LongConstantEntryC, FieldRefConstant, InterfaceMethodRefConstant, MethodRefConstant, NameAndType,
    ClassRefConstant, CodeConstantEntry, FieldDescriptorConstant, IntegerConstant, StringConstant)
  
  def typeTransformations = Seq(SelectInnerClassC, TypeVariable, TypeAbstraction, WildcardTypeArgument, ExtendsTypeArgument,
    SuperTypeArgument, TypeApplication, MethodTypeC) ++
    Seq(ObjectTypeC, ArrayTypeC, ByteTypeC, FloatTypeC, CharTypeC, BooleanTypeC, DoubleTypeC, LongTypeC, VoidTypeC, IntTypeC,
      ShortTypeC, TypeSkeleton)

  def spliceBeforeTransformations(implicits: Seq[Particle], splice: Seq[Particle]): Seq[Particle] = {
    val implicitsSet = implicits.toSet
    javaCompilerTransformations.filter(t => !implicitsSet.contains(t)) ++ splice ++ implicits
  }

  def spliceAfterTransformations(implicits: Seq[Particle], splice: Seq[Particle]): Seq[Particle] = {
    val implicitsSet = implicits.toSet
    implicits ++ splice ++ javaCompilerTransformations.filter(t => !implicitsSet.contains(t))
  }

  def getPrettyPrintJavaToByteCodeCompiler = {
    new CompilerFromParticles(spliceBeforeTransformations(JavaCompiler.byteCodeTransformations, Seq(new PrettyPrint)))
  }
}



