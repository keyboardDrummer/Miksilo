package deltas.javac

import core.deltas._
import deltas.PrettyPrint
import deltas.bytecode._
import deltas.javaPlus.{ExpressionMethodDelta, ReorderMembersDelta}
import deltas.javac.classes.{NewToByteCodeDelta, SelectFieldToByteCodeDelta}
import deltas.javac.constructor._
import deltas.javac.expressions._
import deltas.javac.expressions.additive._
import deltas.javac.expressions.equality.EqualityToByteCodeDelta
import deltas.javac.expressions.literals._
import deltas.javac.expressions.postfix.PostFixIncrementToByteCodeDelta
import deltas.javac.expressions.prefix.LogicalNotToByteCode
import deltas.javac.expressions.relational.{GreaterThanToByteCodeDelta, LessThanToByteCodeDelta}
import deltas.javac.methods.{AssignmentToByteCodeDelta, BlockLanguageDelta, ReturnExpressionToByteCodeDelta, ReturnVoidToByteCodeDelta, VariableToByteCodeDelta}
import deltas.javac.methods.call.CallStaticOrInstanceDelta
import deltas.javac.statements._
import deltas.trivia.{SlashStarBlockCommentsDelta, StoreTriviaDelta, TriviaInsideNode}

object JavaToByteCodeLanguage {

  def getJava: LanguageFromDeltas = LanguageFromDeltas(Seq(ParseUsingTextualGrammar()) ++ javaCompilerDeltas)

  def prettyPrintJavaDeltas: Seq[Delta] = Seq(PrettyPrint()) ++ javaCompilerDeltas

  def allDeltas: Set[Delta] = javaCompilerDeltas.toSet ++
    Set(ConstantPoolIndices, SlashStarBlockCommentsDelta, StoreTriviaDelta,
      TriviaInsideNode, ExpressionMethodDelta, BlockLanguageDelta, ReorderMembersDelta,
      ParseUsingTextualGrammar())

  def javaCompilerDeltas: Seq[Delta] = {
      Seq(NewToByteCodeDelta, ThisCallToByteCodeDelta, SuperCallToByteCodeExpression,
        ImplicitThisForPrivateMemberSelectionDelta, JavaClassToByteCodeDelta) ++
      javaMethod
  }

  def javaMethod: Seq[Delta] =  Seq(ReturnVoidToByteCodeDelta,
    ReturnExpressionToByteCodeDelta, CallStaticOrInstanceDelta,
      SelectFieldToByteCodeDelta) ++ blockWithVariables

  def blockWithVariables: Seq[Delta] = Seq(AssignmentToByteCodeDelta, VariableToByteCodeDelta) ++
    Seq(PostFixIncrementToByteCodeDelta, LocalDeclarationDeltaToByteCode) ++ simpleBlock

  def simpleBlock: Seq[Delta] =
    Seq(GotoToByteCodeDelta, LabelToByteCodeDelta,
      BlockToByteCodeDelta,
      IfThenToByteCodeDelta,
    ExpressionAsStatementToByteCodeDelta) ++ javaSimpleExpression

  def javaSimpleExpression: Seq[Delta] = Seq(TernaryToByteCodeDelta, EqualityToByteCodeDelta,
    LessThanToByteCodeDelta, GreaterThanToByteCodeDelta,
    AdditionToByteCodeDelta,
    SubtractionToByteCodeDelta,
    LongLiteralToByteCodeDelta,
    BooleanLiteralToByteCodeDelta, IntLiteralToByteCodeDelta,
    NullToByteCodeDelta, LogicalNotToByteCode) ++
    Delta.spliceAndFilterTop(JavaLanguage.deltas, ExtendedByteCode.allByteCodeDeltas) // Removes the duplicate type deltas

  def spliceBeforeTransformations(bottom: Seq[Delta], splice: Seq[Delta]): Seq[Delta] =
    Delta.spliceAndFilterTop(getJava.topToBottom, bottom, splice)
}






