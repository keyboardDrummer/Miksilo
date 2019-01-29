package deltas.javac

import core.deltas._
import core.language.Language
import core.smarts.SolveConstraintsDelta
import deltas.bytecode._
import deltas.expression._
import deltas.javaPlus.{ExpressionMethodDelta, ReorderMembersDelta}
import deltas.javac.ByteCodeLanguage.byteCodeDeltas
import deltas.javac.classes._
import deltas.javac.classes.skeleton.{FullyQualifyTypeReferences, JavaClassDelta}
import deltas.javac.constructor._
import deltas.javac.expressions._
import deltas.javac.expressions.additive._
import deltas.javac.expressions.equality.EqualityToByteCodeDelta
import deltas.javac.expressions.literals._
import deltas.javac.expressions.postfix.PostFixIncrementToByteCodeDelta
import deltas.javac.expressions.prefix.LogicalNotToByteCode
import deltas.javac.expressions.relational.{GreaterThanToByteCodeDelta, LessThanToByteCodeDelta}
import deltas.javac.methods._
import deltas.javac.methods.call.{CallDelta, CallStaticOrInstanceDelta}
import deltas.javac.statements._
import deltas.statement._
import deltas.statement.assignment.{AddAssignmentDelta, AssignToVariable, AssignmentPrecedence, SimpleAssignmentDelta}
import deltas.trivia.{SlashStarBlockCommentsDelta, StoreTriviaDelta, TriviaInsideNode}
import deltas.{PrettyPrint, RemovePhasesAfterSolveConstraints}

object JavaToByteCodeLanguage {

  def getJavaFrontend: LanguageFromDeltas = LanguageFromDeltas(Seq(RemovePhasesAfterSolveConstraints) ++
    Seq(ParseUsingTextualGrammar) ++ javaCompilerDeltas)

  def getJava: LanguageFromDeltas = LanguageFromDeltas(Seq(ParseUsingTextualGrammar) ++ javaCompilerDeltas)

  def prettyPrintJavaDeltas: Seq[Delta] = Seq(PrettyPrint()) ++ javaCompilerDeltas

  def allDeltas: Set[Delta] = javaCompilerDeltas.toSet ++
    Set(ConstantPoolIndices, SlashStarBlockCommentsDelta, StoreTriviaDelta,
      TriviaInsideNode, ExpressionMethodDelta, BlockLanguageDelta, ReorderMembersDelta,
      ParseUsingTextualGrammar)

  def javaCompilerDeltas: Seq[Delta] = {
    Seq(ClassifyTypeIdentifiers, DefaultConstructorDelta, ImplicitSuperConstructorCall, ImplicitObjectSuperClass,
      NewToByteCodeDelta, NewDelta, FieldDeclarationWithInitializer, ConstructorDelta) ++
      Seq(ThisCallExpression, SuperCallExpression) ++ fields ++ imports ++
      javaMethod
  }

  def imports = Seq(ImplicitJavaLangImport, WildcardImportDelta, BasicImportDelta)
  def fields = Seq(FieldDeclarationDelta, AssignToMemberDelta)

  val noVariableSyntaxSugarStatements = Seq(IfThenElseToIfThenAndGotoDelta, ForLoopContinueDelta, ForLoopDelta, BlockAsStatementDelta, WhileBreakDelta, WhileContinueDelta, WhileLoopDelta)
  private val syntaxSugarStatements = noVariableSyntaxSugarStatements ++ Seq(LocalDeclarationWithInitializerDelta)
  def javaMethod: Seq[Delta] = Delta.spliceAndFilterBottom(syntaxSugarStatements, //Desugar first because ImplicitThisForPrivateMemberSelection requires variables analysis.)
    Seq(ImplicitReturnAtEndOfMethod, ReturnExpressionToByteCodeDelta, ReturnExpressionDelta, ReturnVoidDelta, CallStaticOrInstanceDelta,
      SelectFieldToByteCodeDelta, SelectFieldDelta) ++ blockWithVariables)

  def blockWithVariables: Seq[Delta] = Seq(LocalDeclarationWithInitializerDelta, LocalDeclarationDeltaToByteCode, LocalDeclarationDelta,
    PostFixIncrementToByteCodeDelta, PostFixIncrementDelta,
    AddAssignmentDelta, AssignToVariable, AssignmentToByteCodeDelta, SimpleAssignmentDelta,
    AssignmentPrecedence, VariableToByteCodeDelta) ++
    Seq(SolveConstraintsDelta,
      ImplicitThisForPrivateMemberSelectionDelta, ThisVariableDelta, MethodDelta, AccessibilityFieldsDelta,
      CallVariableDelta, VariableDelta, CallDelta, MemberSelectorDelta) ++ javaClassSkeleton

  def javaClassSkeleton: Seq[Delta] = Seq(FullyQualifyTypeReferences, JavaClassDelta) ++ simpleBlock

  def simpleBlock: Seq[Delta] = noVariableSyntaxSugarStatements ++
    Seq(GotoToByteCodeDelta, LabelToByteCodeDelta,
      BlockToByteCodeDelta,
      IfThenToByteCodeDelta,
    ExpressionAsStatementToByteCodeDelta, StatementDelta) ++ javaSimpleExpression

  def javaSimpleExpression: Seq[Delta] = Seq(TernaryToByteCodeDelta, EqualityToByteCodeDelta,
    LessThanToByteCodeDelta, GreaterThanToByteCodeDelta,
    AdditionToByteCodeDelta,
    SubtractionToByteCodeDelta,
    LongLiteralToByteCodeDelta,
    BooleanLiteralToByteCodeDelta, IntLiteralToByteCodeDelta,
    NullToByteCodeDelta, LogicalNotToByteCode) ++ JavaLanguage.deltas ++ ExtendedByteCode.allByteCodeDeltas

  def getPrettyPrintJavaToByteCodeCompiler: Language = {
    LanguageFromDeltas(Seq(ParseUsingTextualGrammar) ++ spliceBeforeTransformations(byteCodeDeltas, Seq(new PrettyPrint)))
  }

  def spliceBeforeTransformations(bottom: Seq[Delta], splice: Seq[Delta]): Seq[Delta] =
    Delta.spliceAndFilterTop(getJava.topToBottom, bottom, splice)
}






