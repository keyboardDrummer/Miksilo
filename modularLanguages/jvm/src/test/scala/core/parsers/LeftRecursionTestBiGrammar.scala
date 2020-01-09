package core.parsers

import _root_.core.bigrammar.SelectGrammar
import deltas.ClearPhases
import deltas.expression.relational.{EqualsComparisonDelta, RelationalPrecedenceDelta}
import deltas.expression.{ExpressionDelta, PostFixIncrementDelta, VariableDelta}
import deltas.javac.classes.SelectFieldDelta
import deltas.javac.methods.MemberSelectorDelta
import deltas.javac.methods.call.{CallDelta, CallMemberDelta}
import deltas.javac.{CallVariableDelta, JavaLanguage}
import deltas.statement.assignment.{AssignToVariable, AssignmentPrecedence, SimpleAssignmentDelta}
import deltas.trivia.{SlashStarBlockCommentsDelta, StoreTriviaDelta, TriviaInsideNode}
import org.scalatest.funsuite.AnyFunSuite
import util.{LanguageTest, TestLanguageBuilder}

class LeftRecursionTestBiGrammar extends AnyFunSuite {

  test("recursion detector caching regression") {
    val utils = new LanguageTest(TestLanguageBuilder.buildWithParser(Seq(ClearPhases,
      TriviaInsideNode, StoreTriviaDelta, SlashStarBlockCommentsDelta, new SelectGrammar(ExpressionDelta.FirstPrecedenceGrammar)) ++
      JavaLanguage.javaSimpleExpression))
    assert(utils.compile("2 + 1").diagnostics.isEmpty)
    assert(utils.compile("/* Hello */ 2").diagnostics.isEmpty)
    assert(utils.compile("/* Hello */ 2 + 1").diagnostics.isEmpty)
  }

  //De outer ternary heeft een seed nodig. Die seed krijgt < als root, die meelift op een andere fixpoint, relational ofzo.
  //De seed wordt uiteindelijk iets waarbij 2?1:y de ternary wordt, maar daarvoor moet het gehaal wel in een assignment genest, zijn en die =<value> ontbreekt dan.
  test("moving ternary") {
    val input = """x<2?1:y"""
    val utils = new LanguageTest(TestLanguageBuilder.buildWithParser(
      Seq(ClearPhases, new SelectGrammar(ExpressionDelta.FirstPrecedenceGrammar)) ++ JavaLanguage.deltas))
    assert(utils.compile(input).diagnostics.isEmpty)
  }

  test("moving ternary 2") {
    val input = """index < 2 ? 1 : 2"""
    val utils = new LanguageTest(TestLanguageBuilder.buildWithParser(
      Seq(ClearPhases, new SelectGrammar(ExpressionDelta.FirstPrecedenceGrammar)) ++ JavaLanguage.deltas))
    assert(utils.compile(input).diagnostics.isEmpty) //index < 2 ? 1 : 2
  }

  test("fibonacci regression") {
    val input = "System.out.print(Fibonacci.fibonacci(x))"
    val language = new LanguageTest(TestLanguageBuilder.buildWithParser(Seq(ClearPhases, new SelectGrammar(ExpressionDelta.FirstPrecedenceGrammar)) ++
      Seq(CallMemberDelta, SelectFieldDelta, MemberSelectorDelta) ++ Seq(
      CallVariableDelta, CallDelta) ++ Seq(
      SimpleAssignmentDelta,
      AssignmentPrecedence, VariableDelta) ++ Seq(EqualsComparisonDelta,
      RelationalPrecedenceDelta, ExpressionDelta) ++ JavaLanguage.types
    ))

    val result = language.compile(input)
    assert(result.diagnostics.isEmpty)
  }

  test("postfix regression") {
    val input = """System.out.print(x++)""".stripMargin
    val language = new LanguageTest(TestLanguageBuilder.buildWithParser(Seq(ClearPhases, new SelectGrammar(ExpressionDelta.FirstPrecedenceGrammar)) ++
      Seq(AssignToVariable, PostFixIncrementDelta, CallMemberDelta, SelectFieldDelta, MemberSelectorDelta) ++ Seq(
      CallVariableDelta, CallDelta) ++ Seq(
      SimpleAssignmentDelta,
      AssignmentPrecedence, VariableDelta) ++ Seq(EqualsComparisonDelta,
      RelationalPrecedenceDelta, ExpressionDelta) ++ JavaLanguage.types
    ))
    val result = language.compile(input)
    assert(result.diagnostics.isEmpty)
  }

  test("postfix regression 2") {
    val input = """System.out.print(x++)""".stripMargin
    val utils = new LanguageTest(TestLanguageBuilder.buildWithParser(Seq(ClearPhases, new SelectGrammar(ExpressionDelta.FirstPrecedenceGrammar)) ++
      JavaLanguage.deltas))
    assert(utils.compile(input).diagnostics.isEmpty)
  }
}
