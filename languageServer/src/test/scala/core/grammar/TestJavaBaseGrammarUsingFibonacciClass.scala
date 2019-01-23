package core.grammar

import core.bigrammar.TestLanguageGrammarUtils
import core.language.node.{Node, NodeComparer}
import deltas.bytecode.types._
import deltas.expression.additive.{AdditionDelta, SubtractionDelta}
import deltas.expression.relational.LessThanDelta
import deltas.expression.{ExpressionDelta, IntLiteralDelta, TernaryDelta, VariableDelta}
import deltas.javac.JavaLanguage
import deltas.javac.classes.skeleton.JavaClassSkeleton
import deltas.javac.methods._
import deltas.javac.methods.call.CallDelta
import deltas.javac.statements.ExpressionAsStatementDelta
import deltas.statement.BlockDelta
import deltas.statement.assignment.AddAssignmentDelta
import deltas.trivia.SlashStarBlockCommentsDelta

class TestJavaBaseGrammarUsingFibonacciClass
  extends TestLanguageGrammarUtils(JavaLanguage.javaCompilerDeltas.filter(p => p != SlashStarBlockCommentsDelta))
{

  test("BasicClass") {
    val input = "package bla; class Help {}"
    val result = TestLanguageGrammarUtils.parse(input)
    val expectation = JavaClassSkeleton.neww(Seq("bla"), "Help")
    assertResult(expectation)(result)
  }

  test("MainExpression") {
    val input = "System.out.print(fibonacci(5))"
    val result: Any = getExpressionGrammarResult(input)
    val expectation = CallDelta.neww(MemberSelectorDelta.neww(MemberSelectorDelta.neww(VariableDelta.neww("System"), "out"), "print"),
      Seq(CallDelta.neww(VariableDelta.neww("fibonacci"), Seq(IntLiteralDelta.neww(5)))))
    assertResult(expectation)(result)
  }

  test("FibonacciExpression") {
    val input = "index < 2 ? 1 : fibonacci(index-1) + fibonacci(index-2)"
    val result: Any = getExpressionGrammarResult(input)

    val expectation: Node = getFibonacciExpression
    assertResult(expectation)(result)
  }

  test("ParseLessThanInsideTernary") {
    val input = "1 < 2 ? 3 : 4"
    val result: Any = getExpressionGrammarResult(input)

    val expectation: Node = TernaryDelta.ternary(LessThanDelta.neww(IntLiteralDelta.neww(1), IntLiteralDelta.neww(2)),
      IntLiteralDelta.neww(3), IntLiteralDelta.neww(4))
    assertResult(expectation)(result)
  }

  test("LessThan") {
    val input = "index < 2"
    val result: Any = getExpressionGrammarResult(input)
    val expectation: Node = LessThanDelta.neww(VariableDelta.neww("index"), IntLiteralDelta.neww(2))
    assertResult(expectation)(result)
  }

  test("Addition") {
    val input = "index + 1"
    val result: Any = getExpressionGrammarResult(input)
    val expectation: Node = AdditionDelta.neww(VariableDelta.neww("index"), IntLiteralDelta.neww(1))
    assertResult(expectation)(result)
  }

  def getExpressionGrammarResult(input: String): Any = {
    val result: Any = parse(input, ExpressionDelta.FirstPrecedenceGrammar)
    result
  }

  test("Subtraction") {
    val input = "index - 1"
    val result: Any = getExpressionGrammarResult(input)
    val expectation: Node = SubtractionDelta.neww(VariableDelta.neww("index"), IntLiteralDelta.neww(1))
    assertResult(expectation)(result)
  }

  test("Ternary") {
    val input = "1 ? 2 : 3"
    val result: Any = getExpressionGrammarResult(input)
    val expectation: Node = TernaryDelta.ternary(IntLiteralDelta.neww(1), IntLiteralDelta.neww(2), IntLiteralDelta.neww(3))
    assertResult(expectation)(result)
  }

  test("IncrementAssignment") {
    val input = "x += 1"
    val result: Any = getExpressionGrammarResult(input)
    val expectation = AddAssignmentDelta.neww(VariableDelta.neww("x"), IntLiteralDelta.neww(1))
    assertResult(expectation)(result)
  }

  test("FibonacciMainMethod") {
    val input = "public static void main(java.lang.String[] args) { System.out.print(fibonacci(5)); }"
    val result = getMethodGrammarResult(input)

    val expectation = getMainMethod

    assert(NodeComparer(takeAllRightKeys = false).deepEquality(expectation, result))
  }

  def getMethodGrammarResult(input: String): Any = {
    val result = TestLanguageGrammarUtils.parse(input, MethodDelta.MethodGrammar)
    result
  }

  test("FibonacciMethod") {
    val input = "public static int fibonacci(int index) { return index < 2 ? 1 : fibonacci(index-1) + fibonacci(index-2); }"
    val result: Any = getMethodGrammarResult(input)

    val expectation = getFibonacciMethod
    assert(NodeComparer(takeAllRightKeys = false).deepEquality(expectation, result))
  }

  def getMainMethod: Node = {
    val fibonacciCall = CallDelta.neww(VariableDelta.neww("fibonacci"), Seq(IntLiteralDelta.neww(5)))
    val printCall = CallDelta.neww(MemberSelectorDelta.neww(MemberSelectorDelta.neww(VariableDelta.neww("System"), "out"), "print"),
      Seq(fibonacciCall))
    MethodDelta.neww("main", VoidTypeDelta.voidType, Seq(MethodParameters.neww("args", ArrayTypeDelta.neww(QualifiedObjectTypeDelta.stringType))),
      BlockDelta.neww(Seq(ExpressionAsStatementDelta.create(printCall))), static = true, AccessibilityFieldsDelta.PublicVisibility)
  }

  def getFibonacciMethod: Node = {
    MethodDelta.neww("fibonacci", IntTypeDelta.intType, Seq(MethodParameters.neww("index", IntTypeDelta.intType)),
      BlockDelta.neww(Seq(ReturnExpressionDelta.neww(getFibonacciExpression))), static = true, AccessibilityFieldsDelta.PublicVisibility)
  }

  def getFibonacciExpression: Node = {
    val condition = LessThanDelta.neww(VariableDelta.neww("index"), IntLiteralDelta.neww(2))
    val firstCall = CallDelta.neww(VariableDelta.neww("fibonacci"), Seq(SubtractionDelta.neww(VariableDelta.neww("index"), IntLiteralDelta.neww(1))))
    val secondCall = CallDelta.neww(VariableDelta.neww("fibonacci"), Seq(SubtractionDelta.neww(VariableDelta.neww("index"), IntLiteralDelta.neww(2))))
    val expectation = TernaryDelta.ternary(condition, IntLiteralDelta.neww(1), AdditionDelta.neww(firstCall, secondCall))
    expectation
  }
}