package core.grammar

import core.bigrammar.TestGrammarUtils
import core.particles.node.Node
import org.junit.{Assert, Test}
import transformations.javac.classes._
import transformations.javac.expressions._
import transformations.javac.expressions.additive.{AdditionC, SubtractionC}
import transformations.javac.expressions.literals.IntLiteralC
import transformations.javac.expressions.relational.LessThanC
import transformations.javac.methods._
import transformations.javac.methods.assignment.IncrementAssignmentC
import transformations.javac.statements.ExpressionAsStatementC
import transformations.types.{ArrayTypeC, IntTypeC, ObjectTypeC, VoidTypeC}

class TestJavaBaseGrammarUsingFibonacciClass {

  @Test
  def testBasicClass() {
    val input = "package bla; class Help {}"
    val result = TestGrammarUtils.getGrammarResult(input)
    val expectation = JavaClassSkeleton.clazz(Seq("bla"), "Help")
    Assert.assertEquals(expectation, result)
  }

  @Test
  def testMainExpression() {

    val input = "System.out.print(fibonacci(5))"
    val result: Any = getExpressionGrammarResult(input)
    val expectation = CallC.call(MemberSelector.selector(MemberSelector.selector(VariableC.variable("System"), "out"), "print"),
      Seq(CallC.call(VariableC.variable("fibonacci"), Seq(IntLiteralC.literal(5)))))
    Assert.assertEquals(expectation, result)
  }

  @Test
  def testFibonacciExpression() {
    val input = "index < 2 ? 1 : fibonacci(index-1) + fibonacci(index-2)"
    val result: Any = getExpressionGrammarResult(input)

    val expectation: Node = getFibonacciExpression
    Assert.assertEquals(expectation, result)
  }

  @Test
  def testParseLessThanInsideTernary() {
    val input = "1 < 2 ? 3 : 4"
    val result: Any = getExpressionGrammarResult(input)

    val expectation: Node = TernaryC.ternary(LessThanC.lessThan(IntLiteralC.literal(1), IntLiteralC.literal(2)),
      IntLiteralC.literal(3), IntLiteralC.literal(4))
    Assert.assertEquals(expectation, result)
  }

  @Test
  def testLessThan() {
    val input = "index < 2"
    val result: Any = getExpressionGrammarResult(input)
    val expectation: Node = LessThanC.lessThan(VariableC.variable("index"), IntLiteralC.literal(2))
    Assert.assertEquals(expectation, result)
  }

  @Test
  def testAddition() {
    val input = "index + 1"
    val result: Any = getExpressionGrammarResult(input)
    val expectation: Node = AdditionC.addition(VariableC.variable("index"), IntLiteralC.literal(1))
    Assert.assertEquals(expectation, result)
  }

  def getExpressionGrammarResult(input: String): Any = {
    val result: Any = TestGrammarUtils.getGrammarResult(input, ExpressionSkeleton.ExpressionGrammar)
    result
  }


  @Test
  def testSubtraction() {
    val input = "index - 1"
    val result: Any = getExpressionGrammarResult(input)
    val expectation: Node = SubtractionC.subtraction(VariableC.variable("index"), IntLiteralC.literal(1))
    Assert.assertEquals(expectation, result)
  }

  @Test
  def testTernary() {
    val input = "1 ? 2 : 3"
    val result: Any = getExpressionGrammarResult(input)
    val expectation: Node = TernaryC.ternary(IntLiteralC.literal(1), IntLiteralC.literal(2), IntLiteralC.literal(3))
    Assert.assertEquals(expectation, result)
  }

  @Test
  def testIncrementAssignment() {
    val input = "x += 1"
    val result: Any = getExpressionGrammarResult(input)
    val expectation = IncrementAssignmentC.incrementAssignment(VariableC.variable("x"), IntLiteralC.literal(1))
    Assert.assertEquals(expectation, result)
  }

  @Test
  def testFibonacciMainMethod() {
    val input = "public static void main(java.lang.String[] args) { System.out.print(fibonacci(5)); }"
    val result = getMethodGrammarResult(input)

    val expectation = getMainMethod
    Assert.assertEquals(expectation, result)
  }

  def getMethodGrammarResult(input: String): Any = {
    val result = TestGrammarUtils.getGrammarResult(input, MethodC.MethodGrammar)
    result
  }

  @Test
  def testFibonacciMethod() {
    val input = "public static int fibonacci(int index) { return index < 2 ? 1 : fibonacci(index-1) + fibonacci(index-2); }"
    val result: Any = getMethodGrammarResult(input)

    val expectation = getFibonacciMethod
    Assert.assertEquals(expectation, result)
  }

  def getMainMethod: Node = {
    val fibonacciCall = CallC.call(VariableC.variable("fibonacci"), Seq(IntLiteralC.literal(5)))
    val printCall = CallC.call(MemberSelector.selector(MemberSelector.selector(VariableC.variable("System"), "out"), "print"),
      Seq(fibonacciCall))
    MethodC.method("main", VoidTypeC.voidType, Seq(MethodC.parameter("args", ArrayTypeC.arrayType(ObjectTypeC.stringType))),
      Seq(ExpressionAsStatementC.asStatement(printCall)), true, MethodC.PublicVisibility)
  }

  def getFibonacciMethod: Node = {
    MethodC.method("fibonacci", IntTypeC.intType, Seq(MethodC.parameter("index", IntTypeC.intType)),
      Seq(ReturnExpressionC._return(getFibonacciExpression)), static = true, MethodC.PublicVisibility)
  }

  def getFibonacciExpression: Node = {
    val condition = LessThanC.lessThan(VariableC.variable("index"), IntLiteralC.literal(2))
    val firstCall = CallC.call(VariableC.variable("fibonacci"), Seq(SubtractionC.subtraction(VariableC.variable("index"), IntLiteralC.literal(1))))
    val secondCall = CallC.call(VariableC.variable("fibonacci"), Seq(SubtractionC.subtraction(VariableC.variable("index"), IntLiteralC.literal(2))))
    val expectation = TernaryC.ternary(condition, IntLiteralC.literal(1), AdditionC.addition(firstCall, secondCall))
    expectation
  }
}