package transformations.javac

import application.compilerCockpit.PrettyPrint
import core.particles._
import transformations.bytecode._
import transformations.bytecode.additions.{LabelledLocations, PoptimizeC}
import transformations.bytecode.attributes._
import transformations.bytecode.constants._
import transformations.bytecode.coreInstructions._
import transformations.bytecode.coreInstructions.doubles.DoubleReturnInstructionDelta
import transformations.bytecode.coreInstructions.floats.FloatReturnInstructionDelta
import transformations.bytecode.coreInstructions.integers._
import transformations.bytecode.coreInstructions.integers.integerCompare._
import transformations.bytecode.coreInstructions.longs._
import transformations.bytecode.coreInstructions.objects._
import transformations.bytecode.extraBooleanInstructions._
import transformations.bytecode.extraConstants.{QualifiedClassNameConstant, TypeConstant}
import transformations.bytecode.simpleBytecode.{InferredMaxStack, InferredStackFrames, RemoveConstantPool}
import transformations.bytecode.types._
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
import transformations.javac.expressions.relational.{AddRelationalPrecedence, GreaterThanC, LessThanC}
import transformations.javac.methods._
import transformations.javac.methods.assignment.{AssignToVariable, AssignmentPrecedence, AssignmentSkeleton, IncrementAssignmentC}
import transformations.javac.methods.call.CallStaticOrInstanceC
import transformations.javac.statements._
import transformations.javac.statements.locals.{LocalDeclarationC, LocalDeclarationWithInitializerC}
import transformations.javac.types._

object JavaCompiler {

  def getCompiler = new CompilerFromDeltas(javaCompilerTransformations)

  def allTransformations = javaCompilerTransformations ++ Seq(JavaStyleCommentsC, ExpressionMethodC, BlockCompilerC, JavaGotoC)

  def javaCompilerTransformations: Seq[Delta] = {
    Seq(ClassifyTypeIdentifiers, DefaultConstructorC, ImplicitSuperConstructorCall, ImplicitObjectSuperClass,
      NewC, FieldDeclarationWithInitializer, ConstructorC, SelectorReferenceKind, VariableReferenceKind) ++
      Seq(ThisCallExpression, SuperCallExpression, ThisVariable) ++ fields ++ imports ++
      javaMethod
  }

  def imports = Seq(ImplicitJavaLangImport, WildcardImportC, BasicImportC)
  def fields = Seq(FieldDeclaration, AssignToMember)

  def javaMethod = Seq(ForLoopContinueC, JavaGotoC, ForLoopC, LocalDeclarationWithInitializerC) ++
    Seq(ImplicitReturnAtEndOfMethod, ImplicitThisForPrivateMemberSelection, ReturnExpressionC, ReturnVoidC, CallStaticOrInstanceC, SelectField, MemberSelector) ++ methodBlock
  def methodBlock = Seq(LocalDeclarationC, IncrementAssignmentC, AssignToVariable, AssignmentSkeleton,
    AssignmentPrecedence, PostFixIncrementC, VariableC) ++ Seq(MethodC) ++ Seq(JavaClassSkeleton) ++ javaSimpleStatement

  def javaSimpleStatement = Seq(IfThenElseC, IfThenC, WhileContinueC, WhileC, BlockC,
    ExpressionAsStatementC, StatementSkeleton) ++ javaSimpleExpression

  def javaSimpleExpression: Seq[Delta] = Seq(TernaryC, EqualityC,
    AddEqualityPrecedence, LessThanC, GreaterThanC, AddRelationalPrecedence, AdditionC, SubtractionC, AddAdditivePrecedence,
    BooleanLiteralC, LongLiteralC, IntLiteralC, NullC, NotC, ParenthesisC, ExpressionSkeleton) ++ allByteCodeTransformations

  def allByteCodeTransformations = Seq(OptimizeComparisonInstructionsC) ++
    Seq(LessThanInstructionC, GreaterThanInstructionC, NotInstructionC, IntegerEqualsInstructionC, ExpandVirtualInstructionsC) ++
    simpleByteCodeTransformations

  def simpleByteCodeTransformations: Seq[Delta] = Seq(PoptimizeC) ++
    Seq(InferredStackFrames, InferredMaxStack, LabelledLocations, RemoveConstantPool) ++ byteCodeTransformations

  def byteCodeTransformations = byteCodeInstructions ++ byteCodeWithoutInstructions

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

  def byteCodeWithoutInstructions = byteCodeWithoutTextualParser ++ Seq(ParseUsingTextualGrammar)

  def byteCodeWithoutTextualParser: Seq[DeltaWithGrammar] = bytecodeAttributes ++ constantEntryParticles ++
    Seq(ByteCodeMethodInfo, ByteCodeFieldInfo) ++
    typeTransformations ++ Seq(ByteCodeSkeleton)

  val bytecodeAttributes: Seq[DeltaWithGrammar] = Seq(StackMapTableAttribute, LineNumberTable, SourceFileAttribute,
    CodeAttribute, //ExceptionsAttribute, InnerClassesAttribute,
    SignatureAttribute)

  def constantEntryParticles = Seq(QualifiedClassNameConstant, TypeConstant) ++ Seq(MethodTypeConstant, Utf8Constant, DoubleInfoConstant, LongInfoConstant, FieldRefConstant, InterfaceMethodRefConstant, MethodRefConstant, NameAndTypeConstant,
    ClassInfoConstant, IntegerInfoConstant, StringConstant, MethodHandleConstant, MethodType,
    InvokeDynamicConstant)
  
  def typeTransformations = Seq(SelectInnerClassC, TypeVariable, TypeAbstraction, WildcardTypeArgument, ExtendsTypeArgument,
    SuperTypeArgument, TypeApplication, MethodType) ++
    Seq(ObjectTypeDelta, ArrayTypeC, ByteTypeC, FloatTypeC, CharTypeC, BooleanTypeC, DoubleTypeC, LongTypeC, VoidTypeC, IntTypeC,
      ShortTypeC, TypeSkeleton)

  def spliceBeforeTransformations(implicits: Seq[Delta], splice: Seq[Delta]): Seq[Delta] =
    getCompiler.spliceBeforeTransformations(implicits, splice)

  def spliceAfterTransformations(implicits: Seq[Delta], splice: Seq[Delta]): Seq[Delta] =
    getCompiler.spliceAfterTransformations(implicits, splice)

  def getPrettyPrintJavaToByteCodeCompiler = {
    new CompilerFromDeltas(spliceBeforeTransformations(JavaCompiler.byteCodeTransformations, Seq(new PrettyPrint)))
  }
}



