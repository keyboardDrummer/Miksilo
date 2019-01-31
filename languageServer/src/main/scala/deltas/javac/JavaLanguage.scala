package deltas.javac

import core.deltas.Delta
import core.smarts.SolveConstraintsDelta
import deltas.bytecode.types._
import deltas.expression._
import deltas.expression.additive.{AdditionDelta, AdditivePrecedenceDelta, SubtractionDelta}
import deltas.expression.logical.LogicalNotDelta
import deltas.expression.relational.{AddRelationalPrecedenceDelta, EqualsComparisonDelta, GreaterThanDelta, LessThanDelta}
import deltas.javac.classes._
import deltas.javac.classes.skeleton.{FullyQualifyTypeReferences, JavaClassDelta}
import deltas.javac.constructor._
import deltas.javac.expressions.equality.AddEqualityPrecedence
import deltas.javac.expressions.literals.{BooleanLiteralDelta, LongLiteralDelta, NullDelta}
import deltas.javac.methods._
import deltas.javac.methods.call.CallDelta
import deltas.javac.statements._
import deltas.javac.types._
import deltas.statement._
import deltas.statement.assignment.{AddAssignmentDelta, AssignToVariable, AssignmentPrecedence, SimpleAssignmentDelta}

object JavaLanguage {

  def deltas = javaClass

  def javaClass = Seq(ClassifyTypeIdentifiers) ++ // TODO what is ClassifyTypeIdentifiers??
    imports ++ constructors ++ Seq(FullyQualifyTypeReferences,
    ThisVariableDelta, ImplicitObjectSuperClass, JavaClassDelta) ++ fields ++ method

  def constructors = Seq(DefaultConstructorDelta, ImplicitSuperConstructorCall,
    SuperCallExpression, ThisCallExpression, NewDelta, ConstructorDelta)
  def fields = Seq(FieldDeclarationWithInitializer, FieldDeclarationDelta,
    AssignToMemberDelta, SelectFieldDelta, MemberSelectorDelta)
  def imports = Seq(ImplicitJavaLangImport, WildcardImportDelta, BasicImportDelta)

  def method = Seq(ImplicitReturnAtEndOfMethod,
    ReturnExpressionDelta, ReturnVoidDelta,
    CallVariableDelta, CallDelta, MethodDelta, AccessibilityFieldsDelta) ++ blockWithVariables

  def blockWithVariables = {
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
    AddRelationalPrecedenceDelta, AdditionDelta,
    SubtractionDelta, AdditivePrecedenceDelta,
    BooleanLiteralDelta, LongLiteralDelta, IntLiteralDelta,
    NullDelta, LogicalNotDelta, ParenthesisInExpressionDelta, ExpressionDelta, SolveConstraintsDelta) ++ types

  def types: Seq[Delta] = Seq(TypeVariableDelta, TypeAbstraction, WildcardTypeArgument, ExtendsDelta,
    SuperTypeArgument, TypeApplicationDelta, MethodTypeDelta) ++
    Seq(UnqualifiedObjectTypeDelta, QualifiedObjectTypeDelta, ArrayTypeDelta, ByteTypeDelta,
      FloatTypeDelta, CharTypeDelta, BooleanTypeDelta, DoubleTypeDelta, LongTypeDelta, VoidTypeDelta, IntTypeDelta,
      ShortTypeDelta, TypeSkeleton)
}
