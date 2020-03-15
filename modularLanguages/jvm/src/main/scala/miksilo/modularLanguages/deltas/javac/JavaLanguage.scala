package miksilo.modularLanguages.deltas.javac

import miksilo.languageServer.core.language.Language
import miksilo.modularLanguages.core.SolveConstraintsDelta
import miksilo.modularLanguages.core.deltas.{Delta, LanguageFromDeltas, ParseUsingTextualGrammar}
import miksilo.modularLanguages.deltas.HasNameDelta
import miksilo.modularLanguages.deltas.bytecode.ByteCodeLanguage
import miksilo.modularLanguages.deltas.classes.constructor.{ConstructorDelta, DefaultConstructorDelta, ImplicitSuperConstructorCall, SuperCallExpression}
import miksilo.modularLanguages.deltas.expression._
import miksilo.modularLanguages.deltas.expression.additive.{AdditionDelta, AdditivePrecedenceDelta, SubtractionDelta}
import miksilo.modularLanguages.deltas.expression.logical.LogicalNotDelta
import miksilo.modularLanguages.deltas.expression.relational.{EqualsComparisonDelta, GreaterThanDelta, LessThanDelta, RelationalPrecedenceDelta}
import miksilo.modularLanguages.deltas.javac.classes.{AssignToMemberDelta, BasicImportDelta, ClassifyTypeIdentifiers, FieldDeclarationDelta, FieldDeclarationWithInitializer, SelectFieldDelta, ThisVariableDelta, WildcardImportDelta}
import miksilo.modularLanguages.deltas.javac.classes.skeleton.{FullyQualifyTypeReferences, JavaClassDelta}
import miksilo.modularLanguages.deltas.javac.constructor._
import miksilo.modularLanguages.deltas.javac.expressions.equality.AddEqualityPrecedence
import miksilo.modularLanguages.deltas.javac.expressions.literals._
import miksilo.modularLanguages.deltas.javac.methods.{AccessibilityFieldsDelta, ImplicitReturnAtEndOfMethod, MemberSelectorDelta, ReturnExpressionDelta, ReturnVoidDelta}
import miksilo.modularLanguages.deltas.javac.methods.call.CallMemberDelta
import miksilo.modularLanguages.deltas.javac.statements.{ExpressionAsStatementDelta, ForLoopContinueDelta, IfThenElseToIfThenAndGotoDelta, WhileBreakDelta}
import miksilo.modularLanguages.deltas.method.MethodDelta
import miksilo.modularLanguages.deltas.method.call.CallDelta
import miksilo.modularLanguages.deltas.statement._
import miksilo.modularLanguages.deltas.statement.assignment.{AddAssignmentDelta, AssignToVariable, AssignmentPrecedence, SimpleAssignmentDelta}

object JavaLanguage {

  def java: Language = LanguageFromDeltas(Seq(ParseUsingTextualGrammar()) ++ deltas)

  def deltas = javaClass

  def javaClass = Seq(ClassifyTypeIdentifiers) ++ // TODO what is ClassifyTypeIdentifiers??
    imports ++ constructors ++ Seq(FullyQualifyTypeReferences,
    ThisVariableDelta, ImplicitObjectSuperClass, JavaStandardLibraryDelta, JavaClassDelta) ++ fields ++ method

  def constructors = Seq(DefaultConstructorDelta, ImplicitSuperConstructorCall,
    SuperCallExpression, ThisCallExpression, NewDelta, ConstructorDelta)

  def fields = Seq(CallMemberDelta, FieldDeclarationWithInitializer, FieldDeclarationDelta,
    AssignToMemberDelta, SelectFieldDelta, MemberSelectorDelta)

  def imports = Seq(ImplicitJavaLangImport, WildcardImportDelta, BasicImportDelta)

  def method = Seq(ImplicitReturnAtEndOfMethod,
    ReturnExpressionDelta, ReturnVoidDelta,
    CallVariableDelta, CallDelta, MethodDelta, AccessibilityFieldsDelta) ++ blockWithVariables

  def blockWithVariables = {
    //ForLoop has to come before declaration with initializer, so it will move the loop initializer to a block context.
    val movedDeltas = Seq(ForLoopContinueDelta, ForLoopDelta)

    val aboveForLoop = movedDeltas ++ Seq(
      LocalDeclarationWithInitializerDelta,
      LocalDeclarationDelta,
      PostFixIncrementDelta,
      AddAssignmentDelta, AssignToVariable, SimpleAssignmentDelta,
      AssignmentPrecedence, VariableDelta)

    Delta.spliceAndFilterBottom(aboveForLoop, simpleBlock)
  }

  def simpleBlock: Seq[Delta] = Seq(IfThenElseToIfThenAndGotoDelta, ForLoopContinueDelta, ForLoopDelta,
    BlockAsStatementDelta, WhileBreakDelta, WhileContinueDelta, WhileLoopDelta) ++
    Seq(GotoStatementDelta, LabelStatementDelta,
      IfThenElseDelta, IfThenDelta,
      BlockDelta, ExpressionAsStatementDelta, StatementDelta) ++ javaSimpleExpression

  def javaSimpleExpression: Seq[Delta] = Seq(TernaryDelta, EqualsComparisonDelta,
    AddEqualityPrecedence, LessThanDelta, GreaterThanDelta,
    RelationalPrecedenceDelta, AdditionDelta,
    SubtractionDelta, AdditivePrecedenceDelta,
    BooleanLiteralDelta, LongLiteralDelta, IntLiteralDelta,
    NullDelta, LogicalNotDelta, ParenthesisInExpressionDelta, ExpressionDelta, SolveConstraintsDelta) ++ types

  def types: Seq[Delta] = ByteCodeLanguage.types ++ Seq(HasNameDelta)
}
