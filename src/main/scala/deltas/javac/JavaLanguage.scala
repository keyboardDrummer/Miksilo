package deltas.javac

import application.compilerCockpit.PrettyPrint
import core.deltas._
import core.language.Language
import core.smarts.SolveConstraintsDelta
import deltas.bytecode._
import deltas.bytecode.additions.PoptimizeDelta
import deltas.bytecode.attributes._
import deltas.bytecode.constants._
import deltas.bytecode.coreInstructions._
import deltas.bytecode.coreInstructions.doubles.DoubleReturnInstructionDelta
import deltas.bytecode.coreInstructions.floats.FloatReturnInstructionDelta
import deltas.bytecode.coreInstructions.integers._
import deltas.bytecode.coreInstructions.integers.integerCompare._
import deltas.bytecode.coreInstructions.longs._
import deltas.bytecode.coreInstructions.objects._
import deltas.bytecode.extraBooleanInstructions._
import deltas.bytecode.extraConstants.{QualifiedClassNameConstantDelta, TypeConstant}
import deltas.bytecode.simpleBytecode.{InferredMaxStack, InferredStackFrames, InlineConstantPool, LabelledLocations}
import deltas.bytecode.types._
import deltas.javaPlus.ExpressionMethodDelta
import deltas.javac.classes._
import deltas.javac.classes.skeleton.{FullyQualifyTypeReferences, JavaClassSkeleton}
import deltas.javac.constructor._
import deltas.javac.expressions._
import deltas.javac.expressions.additive.{AddAdditivePrecedence, AdditionDelta, SubtractionDelta}
import deltas.javac.expressions.equality.{AddEqualityPrecedence, EqualityDelta}
import deltas.javac.expressions.literals.{BooleanLiteralDelta, IntLiteralDelta, LongLiteralDelta, NullDelta}
import deltas.javac.expressions.postfix.PostFixIncrementDelta
import deltas.javac.expressions.prefix.NotDelta
import deltas.javac.expressions.relational.{AddRelationalPrecedence, GreaterThanDelta, LessThanDelta}
import deltas.javac.methods._
import deltas.javac.methods.assignment.{AssignToVariable, AssignmentPrecedence, AssignmentSkeleton, IncrementAssignmentDelta}
import deltas.javac.methods.call.CallStaticOrInstanceDelta
import deltas.javac.statements._
import deltas.javac.statements.locals.{LocalDeclarationDelta, LocalDeclarationWithInitializerDelta}
import deltas.javac.trivia.{JavaStyleCommentsDelta, StoreTriviaDelta, TriviaInsideNode}
import deltas.javac.types._

object JavaLanguage {

  def getJava: Language = Delta.buildLanguage(javaCompilerDeltas)

  def prettyPrintJavaDeltas: Seq[Delta] = Seq(PrettyPrint()) ++ javaCompilerDeltas

  def allDeltas: Set[Delta] = javaCompilerDeltas.toSet ++
    Set(ConstantPoolIndices, JavaStyleCommentsDelta, StoreTriviaDelta,
      TriviaInsideNode, ExpressionMethodDelta, BlockLanguageDelta)

  def javaCompilerDeltas: Seq[Delta] = {
    Seq(ClassifyTypeIdentifiers, DefaultConstructorDelta, ImplicitSuperConstructorCall, ImplicitObjectSuperClass,
      NewDelta, FieldDeclarationWithInitializer, ConstructorDelta, SelectorReferenceKind, VariableReferenceKind) ++
      Seq(ThisCallExpression, SuperCallExpression, ThisVariableDelta) ++ fields ++ imports ++
      javaMethod
  }

  def imports = Seq(ImplicitJavaLangImport, WildcardImportDelta, BasicImportDelta)
  def fields = Seq(FieldDeclarationDelta, AssignToMember)

  val noVariableSyntaxSugarStatements = Seq(ForLoopContinueDelta, ForLoopDelta, BlockAsStatementDelta, WhileBreakDelta, WhileContinueDelta, WhileLoopDelta)
  private val syntaxSugarStatements = noVariableSyntaxSugarStatements ++ Seq(LocalDeclarationWithInitializerDelta)
  def javaMethod: Seq[Delta] = Delta.spliceAndFilterBottom(syntaxSugarStatements, //Desugar first because ImplicitThisForPrivateMemberSelection requires variables analysis.)
    Seq(ImplicitReturnAtEndOfMethod, ImplicitThisForPrivateMemberSelection, ReturnExpressionDelta, ReturnVoidDelta, CallStaticOrInstanceDelta, SelectField, MemberSelectorDelta) ++ blockWithVariables)

  def blockWithVariables: Seq[Delta] = Seq(LocalDeclarationWithInitializerDelta, LocalDeclarationDelta, IncrementAssignmentDelta, AssignToVariable, AssignmentSkeleton,
    AssignmentPrecedence, PostFixIncrementDelta, VariableDelta) ++ Seq(MethodDelta, AccessibilityFieldsDelta) ++ Seq(SolveConstraintsDelta) ++ javaClassSkeleton

  def javaClassSkeleton: Seq[Delta] = Seq(FullyQualifyTypeReferences, JavaClassSkeleton) ++ simpleBlock //TODO What is JavaClassSkeleton doing here?

  def simpleBlock: Seq[Delta] = noVariableSyntaxSugarStatements ++ Seq(JavaGotoDelta, IfThenElseDelta, IfThenDelta, BlockDelta,
    ExpressionAsStatementDelta, StatementSkeleton) ++ javaSimpleExpression

  def javaSimpleExpression: Seq[Delta] = Seq(TernaryDelta, EqualityDelta,
    AddEqualityPrecedence, LessThanDelta, GreaterThanDelta, AddRelationalPrecedence, AdditionDelta, SubtractionDelta, AddAdditivePrecedence,
    BooleanLiteralDelta, LongLiteralDelta, IntLiteralDelta, NullDelta, NotDelta, ParenthesisDelta, ExpressionSkeleton) ++ allByteCodeDeltas

  def allByteCodeDeltas: Seq[Delta] = Seq(OptimizeComparisonInstructionsDelta) ++
    Seq(LessThanInstructionDelta, GreaterThanInstructionDelta, NotInstructionDelta, IntegerEqualsInstructionDelta, ExpandVirtualInstructionsDelta) ++
    simpleByteCodeTransformations

  def simpleByteCodeTransformations: Seq[Delta] = Seq(PoptimizeDelta) ++
    Seq(InferredStackFrames, InferredMaxStack, LabelledLocations, InlineConstantPool) ++ byteCodeDeltas

  def byteCodeDeltas: Seq[Delta] = byteCodeInstructions ++ byteCodeWithoutInstructions

  def byteCodeInstructions: Seq[InstructionDelta] = {
    Seq(Pop2Delta, PopDelta, GetStaticDelta, GotoDelta, IfIntegerCompareLessDelta, IfIntegerCompareLessOrEqualDelta,
      IfZeroDelta, IfNotZero, InvokeSpecialDelta, InvokeVirtualDelta, InvokeStaticDelta, NewByteCodeDelta, Duplicate2InstructionDelta, DuplicateInstructionDelta) ++
      objectInstructions ++ Seq(PushNullDelta, StoreIntegerDelta, SubtractIntegerDelta, VoidReturnInstructionDelta,
      SwapInstruction, GetFieldDelta, PutField) ++
      integerInstructions ++ longInstructions ++ floatInstructions ++ doubleInstructions
  }

  def objectInstructions = Seq(LoadAddressDelta, AddressReturnInstructionDelta, StoreAddressDelta)

  def doubleInstructions = Seq(DoubleReturnInstructionDelta)

  def floatInstructions = Seq(FloatReturnInstructionDelta)

  def longInstructions = Seq(LongReturnInstructionDelta, AddLongsDelta, CompareLongDelta, PushLongDelta, LoadLongDelta, StoreLongDelta)

  def integerInstructions = Seq(AddIntegersDelta, SmallIntegerConstantDelta, LoadConstantDelta, IncrementIntegerDelta, IntegerReturnInstructionDelta, LoadIntegerDelta, IfIntegerCompareGreaterOrEqualDelta,
    IfIntegerCompareEqualDelta, IfIntegerCompareNotEqualDelta)

  def byteCodeWithoutInstructions: Seq[Delta] = byteCodeWithoutTextualParser ++ Seq(ParseUsingTextualGrammar)

  def byteCodeWithoutTextualParser: Seq[Delta] = bytecodeAttributes ++ constantEntryDeltas ++
    Seq(ByteCodeMethodInfo, ByteCodeFieldInfo) ++
    typeTransformations ++ Seq(ByteCodeSkeleton)

  val bytecodeAttributes: Seq[DeltaWithGrammar] = Seq(StackMapTableAttribute, LineNumberTable, SourceFileAttribute,
    CodeAttributeDelta, //ExceptionsAttribute, InnerClassesAttribute,
    SignatureAttribute)

  def constantEntryDeltas: Seq[Delta] = Seq(QualifiedClassNameConstantDelta, TypeConstant) ++ Seq(MethodTypeConstant, Utf8ConstantDelta, DoubleInfoConstant, LongInfoConstant, FieldRefConstant, InterfaceMethodRefConstant, MethodRefConstant, NameAndTypeConstant,
    ClassInfoConstant, IntegerInfoConstant, StringConstant, MethodHandleConstant, MethodType,
    InvokeDynamicConstant)

  def typeTransformations: Seq[Delta] = Seq(SelectInnerClassDelta, TypeVariableDelta, TypeAbstraction, WildcardTypeArgument, ExtendsDelta,
    SuperTypeArgument, TypeApplicationDelta, MethodType) ++
    Seq(UnqualifiedObjectTypeDelta, QualifiedObjectTypeDelta, ArrayTypeDelta, ByteTypeDelta, FloatTypeDelta, CharTypeDelta, BooleanTypeDelta, DoubleTypeDelta, LongTypeDelta, VoidTypeDelta, IntTypeDelta,
      ShortTypeDelta, TypeSkeleton)

  def spliceBeforeTransformations(implicits: Seq[Delta], splice: Seq[Delta]): Seq[Delta] =
    Delta.spliceAndFilterTop(javaCompilerDeltas, implicits, splice)

  def spliceAfterTransformations(implicits: Seq[Delta], splice: Seq[Delta]): Seq[Delta] =
    Delta.spliceAndFilterBottom(implicits, javaCompilerDeltas, splice)

  def getPrettyPrintJavaToByteCodeCompiler: Language = {
    Delta.buildLanguage(spliceBeforeTransformations(JavaLanguage.byteCodeDeltas, Seq(new PrettyPrint)))
  }
}



