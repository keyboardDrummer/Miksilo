package miksilo.modularLanguages.deltas.javac

import miksilo.modularLanguages.core.deltas._
import miksilo.modularLanguages.deltas.PrettyPrint
import miksilo.modularLanguages.deltas.bytecode._
import miksilo.modularLanguages.deltas.javaPlus.{ExpressionMethodDelta, ReorderMembersDelta}
import miksilo.modularLanguages.deltas.javac.classes.{NewToByteCodeDelta, SelectFieldToByteCodeDelta}
import miksilo.modularLanguages.deltas.javac.constructor._
import miksilo.modularLanguages.deltas.javac.expressions._
import miksilo.modularLanguages.deltas.javac.expressions.additive._
import miksilo.modularLanguages.deltas.javac.expressions.equality.EqualityToByteCodeDelta
import miksilo.modularLanguages.deltas.javac.expressions.literals._
import miksilo.modularLanguages.deltas.javac.expressions.postfix.PostFixIncrementToByteCodeDelta
import miksilo.modularLanguages.deltas.javac.expressions.prefix.LogicalNotToByteCode
import miksilo.modularLanguages.deltas.javac.expressions.relational.{GreaterThanToByteCodeDelta, LessThanToByteCodeDelta}
import miksilo.modularLanguages.deltas.javac.methods.{AssignmentToByteCodeDelta, BlockLanguageDelta, ReturnExpressionToByteCodeDelta, ReturnVoidToByteCodeDelta, VariableToByteCodeDelta}
import miksilo.modularLanguages.deltas.javac.methods.call.CallStaticOrInstanceDelta
import miksilo.modularLanguages.deltas.javac.statements._
import miksilo.modularLanguages.deltas.trivia.{SlashSlashLineCommentsDelta, SlashStarBlockCommentsDelta, StoreTriviaDelta, TriviaInsideNode}

object JavaToByteCodeLanguage {

  def getJava: LanguageFromDeltas = LanguageFromDeltas(Seq(ParseUsingTextualGrammar()) ++ javaCompilerDeltas)

  def prettyPrintJavaDeltas: Seq[Delta] = Seq(PrettyPrint()) ++ javaCompilerDeltas

  def allDeltas: Set[Delta] = javaCompilerDeltas.toSet ++
    Set(ConstantPoolIndices, SlashSlashLineCommentsDelta, SlashStarBlockCommentsDelta, StoreTriviaDelta,
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






