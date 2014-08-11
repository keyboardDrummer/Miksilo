package core.grammar

import core.transformation._
import core.transformation.grammars.ProgramGrammar
import org.junit.{Assert, Test}
import transformations.javac.base._
import transformations.javac.base.model._
import transformations.javac.expressions._
import transformations.javac.methods.{CallC, ReturnExpressionC, SelectorC, VariableC}
import transformations.javac.types.{ArrayTypeC, IntTypeC, ObjectTypeC, VoidTypeC}

import scala.reflect.io.{File, Path}

class TestJavaBaseGrammarUsingFibonacciClass {

  @Test
  def testBasicClass() {
    val input = "package bla; class Help {}"
    val result = getGrammarResult(input)
    val expectation = JavaClassModel.clazz(Seq("bla"), "Help", Seq.empty[MetaObject], List.empty[JavaImport])
    Assert.assertEquals(expectation, result)
  }

  def getGrammarResult(input: String, grammarTransformer: Any = ProgramGrammar): Any = {
    val parser = TestGrammarUtils.getJavaParser(grammarTransformer)
    val parseResult = parser(input)
    if (parseResult.isEmpty)
      Assert.fail(parseResult.toString)

    val result = parseResult.get
    Assert.assertTrue(result.toString, parseResult.next.atEnd)
    result
  }

  @Test
  def testMainExpression() {

    val input = "System.out.print(fibonacci(5))"
    val result: Any = getExpressionGrammarResult(input)
    val expectation = CallC.call(SelectorC.selector(SelectorC.selector(VariableC.variable("System"), "out"), "print"),
      Seq(CallC.call(VariableC.variable("fibonacci"), Seq(NumberLiteralC.literal(5)))))
    Assert.assertEquals(expectation, result)
  }

  @Test
  def testFibonacciExpression() {
    val input = "index < 2 ? 1 : fibonacci(index-1) + fibonacci(index-2)"
    val result: Any = getExpressionGrammarResult(input)

    val expectation: MetaObject = getFibonacciExpression
    Assert.assertEquals(expectation, result)
  }

  def getExpressionGrammarResult(input: String): Any = {
    val result: Any = getGrammarResult(input, ExpressionC.ExpressionGrammar)
    result
  }

  @Test
  def testParseLessThanInsideTernary() {
    val input = "1 < 2 ? 3 : 4"
    val result: Any = getExpressionGrammarResult(input)

    val expectation: MetaObject = TernaryC.ternary(LessThanC.lessThan(NumberLiteralC.literal(1), NumberLiteralC.literal(2)),
      NumberLiteralC.literal(3), NumberLiteralC.literal(4))
    Assert.assertEquals(expectation, result)
  }

  @Test
  def testLessThan() {
    val input = "index < 2"
    val result: Any = getExpressionGrammarResult(input)
    val expectation: MetaObject = LessThanC.lessThan(VariableC.variable("index"), NumberLiteralC.literal(2))
    Assert.assertEquals(expectation, result)
  }

  @Test
  def testAddition() {
    val input = "index + 1"
    val result: Any = getExpressionGrammarResult(input)
    val expectation: MetaObject = AdditionC.addition(VariableC.variable("index"), NumberLiteralC.literal(1))
    Assert.assertEquals(expectation, result)
  }

  @Test
  def testSubtraction() {
    val input = "index - 1"
    val result: Any = getExpressionGrammarResult(input)
    val expectation: MetaObject = SubtractionC.subtraction(VariableC.variable("index"), NumberLiteralC.literal(1))
    Assert.assertEquals(expectation, result)
  }

  @Test
  def testTernary() {
    val input = "1 ? 2 : 3"
    val result: Any = getExpressionGrammarResult(input)
    val expectation: MetaObject = TernaryC.ternary(NumberLiteralC.literal(1), NumberLiteralC.literal(2), NumberLiteralC.literal(3))
    Assert.assertEquals(expectation, result)
  }

  @Test
  def testFibonacciMainMethod() {
    val input = "public static void main(java.lang.String[] args) { System.out.print(fibonacci(5)); }"
    val result = getMethodGrammarResult(input)

    val expectation = getMainMethod
    Assert.assertEquals(expectation, result)
  }

  def getMainMethod: MetaObject = {
    JavaMethodModel.method("main", VoidTypeC.voidType, Seq(JavaMethodModel.parameter("args", ArrayTypeC.arrayType(ObjectTypeC.stringType))),
      Seq(CallC.call(SelectorC.selector(SelectorC.selector(VariableC.variable("System"), "out"), "print"),
        Seq(CallC.call(VariableC.variable("fibonacci"), Seq(NumberLiteralC.literal(5)))))), true, JavaMethodModel.PublicVisibility)
  }

  @Test
  def testFibonacciMethod() {
    val input = "public static int fibonacci(int index) { return index < 2 ? 1 : fibonacci(index-1) + fibonacci(index-2); }"
    val result: Any = getMethodGrammarResult(input)

    val expectation = getFibonacciMethod
    Assert.assertEquals(expectation, result)
  }

  def getMethodGrammarResult(input: String): Any = {
    val result = getGrammarResult(input, MethodAndClassC.MethodGrammar)
    result
  }

  def getFibonacciMethod: MetaObject = {
    JavaMethodModel.method("fibonacci", IntTypeC.intType, Seq(JavaMethodModel.parameter("index", IntTypeC.intType)),
      Seq(ReturnExpressionC._return(getFibonacciExpression)), static = true, JavaMethodModel.PublicVisibility)
  }

  def getFibonacciExpression: MetaObject = {
    val condition = LessThanC.lessThan(VariableC.variable("index"), NumberLiteralC.literal(2))
    val firstCall = CallC.call(VariableC.variable("fibonacci"), Seq(SubtractionC.subtraction(VariableC.variable("index"), NumberLiteralC.literal(1))))
    val secondCall = CallC.call(VariableC.variable("fibonacci"), Seq(SubtractionC.subtraction(VariableC.variable("index"), NumberLiteralC.literal(2))))
    val expectation = TernaryC.ternary(condition, NumberLiteralC.literal(1), AdditionC.addition(firstCall, secondCall))
    expectation
  }

  @Test
  def testFibonacci() {
    val inputFile = Path("testResources") / "fibonacciWithMain" / "Fibonacci.java"

    val input = File(inputFile).slurp()

    val result = getGrammarResult(input)
    val expectation = JavaClassModel.clazz(Seq("fibonacciWithMain"), "Fibonacci", Seq(getMainMethod, getFibonacciMethod), List.empty[JavaImport])
    Assert.assertEquals(expectation, result)
  }
}