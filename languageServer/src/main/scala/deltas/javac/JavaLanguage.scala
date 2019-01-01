package deltas.javac

import core.deltas._
import core.language.Language
import core.smarts.SolveConstraintsDelta
import deltas.{PrettyPrint, RemovePhasesAfterSolveConstraints}
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
import deltas.expression.IntLiteralDelta
import deltas.expressions.ExpressionDelta
import deltas.javaPlus.{ExpressionMethodDelta, ReorderMembersDelta}
import deltas.javac.classes._
import deltas.javac.classes.skeleton.{FullyQualifyTypeReferences, JavaClassSkeleton}
import deltas.javac.constructor._
import deltas.javac.expressions._
import deltas.javac.expressions.additive._
import deltas.javac.expressions.equality.{AddEqualityPrecedence, EqualityDelta}
import deltas.javac.expressions.literals._
import deltas.javac.expressions.postfix.PostFixIncrementToByteCodeDelta
import deltas.javac.expressions.prefix.NotDelta
import deltas.javac.expressions.relational.{AddRelationalPrecedenceDelta, GreaterThanDelta, GreaterThanToByteCodeDelta, LessThanDelta}
import deltas.javac.methods._
import deltas.javac.methods.assignment._
import deltas.javac.methods.call.CallStaticOrInstanceDelta
import deltas.javac.statements._
import deltas.javac.trivia.{JavaStyleBlockCommentsDelta, StoreTriviaDelta, TriviaInsideNode}
import deltas.javac.types._
import deltas.statement._

//TODO split the compilation to ByteCode from the language definition
object JavaLanguage {

  def getJavaFrontend: LanguageFromDeltas = LanguageFromDeltas(Seq(RemovePhasesAfterSolveConstraints) ++
    Seq(ParseUsingTextualGrammar) ++ javaCompilerDeltas)

  def getJava: LanguageFromDeltas = LanguageFromDeltas(Seq(ParseUsingTextualGrammar) ++ javaCompilerDeltas)

  def prettyPrintJavaDeltas: Seq[Delta] = Seq(PrettyPrint()) ++ javaCompilerDeltas

  def allDeltas: Set[Delta] = javaCompilerDeltas.toSet ++
    Set(ConstantPoolIndices, JavaStyleBlockCommentsDelta, StoreTriviaDelta,
      TriviaInsideNode, ExpressionMethodDelta, BlockLanguageDelta, ReorderMembersDelta)

  def javaCompilerDeltas: Seq[Delta] = {
    Seq(ClassifyTypeIdentifiers, DefaultConstructorDelta, ImplicitSuperConstructorCall, ImplicitObjectSuperClass,
      NewDelta, FieldDeclarationWithInitializer, ConstructorDelta, MemberSelectorAsNamespaceReference, VariableAsNamespaceReference) ++
      Seq(ThisCallExpression, SuperCallExpression) ++ fields ++ imports ++
      javaMethod
  }

  def imports = Seq(ImplicitJavaLangImport, WildcardImportDelta, BasicImportDelta)
  def fields = Seq(FieldDeclarationDelta, AssignToMember)

  val noVariableSyntaxSugarStatements = Seq(IfThenElseToIfThenAndGotoDelta, ForLoopContinueDelta, ForLoopDelta, BlockAsStatementDelta, WhileBreakDelta, WhileContinueDelta, WhileLoopDelta)
  private val syntaxSugarStatements = noVariableSyntaxSugarStatements ++ Seq(LocalDeclarationWithInitializerDelta)
  def javaMethod: Seq[Delta] = Delta.spliceAndFilterBottom(syntaxSugarStatements, //Desugar first because ImplicitThisForPrivateMemberSelection requires variables analysis.)
    Seq(ImplicitReturnAtEndOfMethod, ThisVariableDelta,
      ReturnExpressionToByteCodeDelta, ReturnExpressionDelta, ReturnVoidDelta, CallStaticOrInstanceDelta,
      SelectFieldToByteCodeDelta, SelectFieldDelta, MemberSelectorDelta) ++ blockWithVariables)

  def blockWithVariables: Seq[Delta] = Seq(LocalDeclarationWithInitializerDelta, LocalDeclarationDeltaToByteCode,
    IncrementAssignmentDelta, AssignToVariable, AssignmentToByteCodeDelta, EqualsAssignmentDelta,
    AssignmentPrecedence, PostFixIncrementToByteCodeDelta, VariableToByteCodeDelta) ++
    Seq(MethodDelta, AccessibilityFieldsDelta) ++
    Seq(SolveConstraintsDelta, ImplicitThisForPrivateMemberSelectionDelta) ++ javaClassSkeleton

  def javaClassSkeleton: Seq[Delta] = Seq(FullyQualifyTypeReferences, JavaClassSkeleton) ++ simpleBlock

  def simpleBlock: Seq[Delta] = noVariableSyntaxSugarStatements ++ Seq(GotoToByteCodeDelta, LabelToByteCodeDelta, IfThenElseDelta,
    IfThenToByteCodeDelta, IfThenDelta,
    BlockDelta,
    ExpressionAsStatementDelta, StatementDelta) ++ javaSimpleExpression

  def javaSimpleExpression: Seq[Delta] = Seq(TernaryDelta, EqualityDelta,
    AddEqualityPrecedence, LessThanDelta, GreaterThanToByteCodeDelta, GreaterThanDelta, AddRelationalPrecedenceDelta, AdditionToByteCodeDelta, AdditionDelta,
    SubtractionToByteCodeDelta, SubtractionDelta, AdditivePrecedenceDelta,
    BooleanLiteralToByteCodeDelta, BooleanLiteralDelta, LongLiteralDelta, IntLiteralToByteCodeDelta, IntLiteralDelta, NullDelta, NotDelta, ParenthesisInExpressionDelta, ExpressionDelta) ++ allByteCodeDeltas

  def allByteCodeDeltas: Seq[Delta] = Seq(OptimizeComparisonInstructionsDelta) ++
    Seq(LessThanInstructionDelta, GreaterThanInstructionDelta, NotInstructionDelta, IntegerEqualsInstructionDelta, ExpandVirtualInstructionsDelta) ++
    simpleByteCodeDeltas

  def simpleByteCodeDeltas: Seq[Delta] = Seq(PoptimizeDelta) ++
    Seq(InferredStackFrames, InferredMaxStack, LabelledLocations, InlineConstantPool) ++ byteCodeDeltas

  def byteCodeDeltas: Seq[Delta] = byteCodeInstructions ++ byteCodeWithoutInstructions

  def byteCodeInstructions: Seq[InstructionInstance] = {
    Seq(Pop2Delta, PopDelta, GetStaticDelta, GotoDelta, IfIntegerCompareLessDelta, IfIntegerCompareLessOrEqualDelta,
      IfZeroDelta, IfNotZero, InvokeSpecialDelta, InvokeVirtualDelta, InvokeStaticDelta, NewByteCodeDelta, Duplicate2InstructionDelta, DuplicateInstructionDelta) ++
      objectInstructions ++ Seq(PushNullDelta, StoreIntegerDelta, SubtractIntegerDelta, VoidReturnInstructionDelta,
      SwapInstruction, GetFieldDelta, PutField) ++
      integerInstructions ++ longInstructions ++ floatInstructions ++ doubleInstructions
  }

  def objectInstructions: Seq[InstructionInstance] = Seq(LoadAddressDelta, AddressReturnInstructionDelta, StoreAddressDelta)

  def doubleInstructions: Seq[InstructionInstance] = Seq(DoubleReturnInstructionDelta)

  def floatInstructions: Seq[InstructionInstance] = Seq(FloatReturnInstructionDelta)

  def longInstructions: Seq[InstructionInstance] = Seq(LongReturnInstructionDelta, AddLongsDelta, CompareLongDelta, PushLongDelta, LoadLongDelta, StoreLongDelta)

  def integerInstructions: Seq[InstructionInstance] = Seq(AddIntegersDelta, SmallIntegerConstantDelta,
    LoadConstantDelta, IncrementIntegerDelta, IntegerReturnInstructionDelta, LoadIntegerDelta, IfIntegerCompareGreaterOrEqualDelta,
    IfIntegerCompareEqualDelta, IfIntegerCompareNotEqualDelta)

  def byteCodeWithoutInstructions: Seq[Delta] = byteCodeWithoutTextualParser

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
    LanguageFromDeltas(Seq(ParseUsingTextualGrammar) ++ spliceBeforeTransformations(JavaLanguage.byteCodeDeltas, Seq(new PrettyPrint)))
  }
}



