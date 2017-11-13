package core.grammar

import core.bigrammar.TestCompilerGrammarUtils
import core.particles.node.{ComparisonOptions, Node}
import transformations.bytecode.types.{ArrayTypeC, IntTypeC, ObjectTypeDelta, VoidTypeC}
import transformations.javac.classes.skeleton.JavaClassSkeleton
import transformations.javac.expressions._
import transformations.javac.expressions.additive.{AdditionDelta, SubtractionC}
import transformations.javac.expressions.literals.IntLiteralDelta
import transformations.javac.expressions.relational.LessThanC
import transformations.javac.methods._
import transformations.javac.methods.assignment.IncrementAssignmentC
import transformations.javac.methods.call.CallC
import transformations.javac.statements.ExpressionAsStatementC
import transformations.javac.JavaCompilerDeltas
import transformations.javac.trivia.JavaStyleCommentsDelta

class TestJavaBaseGrammarUsingFibonacciClass
  extends TestCompilerGrammarUtils(JavaCompilerDeltas.javaCompilerTransformations.filter(p => p != JavaStyleCommentsDelta))
{

  test("BasicClass") {
    val input = "package bla; class Help {}"
    val result = TestCompilerGrammarUtils.getGrammarResult(input)
    val expectation = JavaClassSkeleton.clazz(Seq("bla"), "Help")
    assertResult(expectation)(result)
  }

  test("MainExpression") {
    val input = "System.out.print(fibonacci(5))"
    val result: Any = getExpressionGrammarResult(input)
    val expectation = CallC.call(MemberSelector.selector(MemberSelector.selector(VariableC.variable("System"), "out"), "print"),
      Seq(CallC.call(VariableC.variable("fibonacci"), Seq(IntLiteralDelta.literal(5)))))
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

    val expectation: Node = TernaryC.ternary(LessThanC.lessThan(IntLiteralDelta.literal(1), IntLiteralDelta.literal(2)),
      IntLiteralDelta.literal(3), IntLiteralDelta.literal(4))
    assertResult(expectation)(result)
  }

  test("LessThan") {
    val input = "index < 2"
    val result: Any = getExpressionGrammarResult(input)
    val expectation: Node = LessThanC.lessThan(VariableC.variable("index"), IntLiteralDelta.literal(2))
    assertResult(expectation)(result)
  }

  test("Addition") {
    val input = "index + 1"
    val result: Any = getExpressionGrammarResult(input)
    val expectation: Node = AdditionDelta.addition(VariableC.variable("index"), IntLiteralDelta.literal(1))
    assertResult(expectation)(result)
  }

  def getExpressionGrammarResult(input: String): Any = {
    val result: Any = getGrammarResult(input, ExpressionSkeleton.ExpressionGrammar)
    result
  }

  test("Subtraction") {
    val input = "index - 1"
    val result: Any = getExpressionGrammarResult(input)
    val expectation: Node = SubtractionC.subtraction(VariableC.variable("index"), IntLiteralDelta.literal(1))
    assertResult(expectation)(result)
  }

  test("Ternary") {
    val input = "1 ? 2 : 3"
    val result: Any = getExpressionGrammarResult(input)
    val expectation: Node = TernaryC.ternary(IntLiteralDelta.literal(1), IntLiteralDelta.literal(2), IntLiteralDelta.literal(3))
    assertResult(expectation)(result)
  }

  test("IncrementAssignment") {
    val input = "x += 1"
    val result: Any = getExpressionGrammarResult(input)
    val expectation = IncrementAssignmentC.incrementAssignment(VariableC.variable("x"), IntLiteralDelta.literal(1))
    assertResult(expectation)(result)
  }

  test("FibonacciMainMethod") {
    val input = "public static void main(java.lang.String[] args) { System.out.print(fibonacci(5)); }"
    val result = getMethodGrammarResult(input)

    val expectation = getMainMethod

    assert(ComparisonOptions(takeAllRightKeys = false).deepEquality(expectation, result))
  }

  def getMethodGrammarResult(input: String): Any = {
    val result = TestCompilerGrammarUtils.getGrammarResult(input, MethodDelta.MethodGrammar)
    result
  }

  test("FibonacciMethod") {
    val input = "public static int fibonacci(int index) { return index < 2 ? 1 : fibonacci(index-1) + fibonacci(index-2); }"
    val result: Any = getMethodGrammarResult(input)

    val expectation = getFibonacciMethod
    assert(ComparisonOptions(takeAllRightKeys = false).deepEquality(expectation, result))
  }

  def getMainMethod: Node = {
    val fibonacciCall = CallC.call(VariableC.variable("fibonacci"), Seq(IntLiteralDelta.literal(5)))
    val printCall = CallC.call(MemberSelector.selector(MemberSelector.selector(VariableC.variable("System"), "out"), "print"),
      Seq(fibonacciCall))
    MethodDelta.method("main", VoidTypeC.voidType, Seq(MethodDelta.parameter("args", ArrayTypeC.arrayType(ObjectTypeDelta.stringType))),
      Seq(ExpressionAsStatementC.create(printCall)), static = true, MethodDelta.PublicVisibility)
  }

  def getFibonacciMethod: Node = {
    MethodDelta.method("fibonacci", IntTypeC.intType, Seq(MethodDelta.parameter("index", IntTypeC.intType)),
      Seq(ReturnExpressionC._return(getFibonacciExpression)), static = true, MethodDelta.PublicVisibility)
  }

  def getFibonacciExpression: Node = {
    val condition = LessThanC.lessThan(VariableC.variable("index"), IntLiteralDelta.literal(2))
    val firstCall = CallC.call(VariableC.variable("fibonacci"), Seq(SubtractionC.subtraction(VariableC.variable("index"), IntLiteralDelta.literal(1))))
    val secondCall = CallC.call(VariableC.variable("fibonacci"), Seq(SubtractionC.subtraction(VariableC.variable("index"), IntLiteralDelta.literal(2))))
    val expectation = TernaryC.ternary(condition, IntLiteralDelta.literal(1), AdditionDelta.addition(firstCall, secondCall))
    expectation
  }
}