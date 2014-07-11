package core.grammar

import core.transformation._
import org.junit.{Assert, Test}
import transformations.javac._
import transformations.javac.base._
import transformations.javac.base.model.{JavaImport, _}

import scala.reflect.io.{File, Path}

class TestJavaBaseGrammarUsingFibonacciClass {

  val grammarSequence: Seq[GrammarTransformation] =
    JavaCompiler.javaCompilerTransformations.reverse.collect( { case x: GrammarTransformation => x} )


  @Test
  def testBasicClass() {
    val input = "package bla; class Help {}"
    val result = getGrammarResult(input)
    val expectation = JavaClassModel.clazz(Seq("bla"), "Help", Seq.empty[MetaObject], List.empty[JavaImport])
    Assert.assertEquals(expectation, result)
  }

  @Test
  def testMainExpression() {

    val input = "System.out.print(fibonacci(5))"
    val result: Any = getExpressionGrammarResult(input)
    val expectation = JavaBaseModel.call(JavaBaseModel.selector(JavaBaseModel.selector(JavaBaseModel.variable("System"),"out"),"print"),
      Seq(JavaBaseModel.call(JavaBaseModel.variable("fibonacci"),Seq(LiteralC.literal(5)))))
    Assert.assertEquals(expectation, result)
  }

  @Test
  def testFibonacciExpression() {
    val input = "index < 2 ? 1 : fibonacci(index-1) + fibonacci(index-2)"
    val result: Any = getExpressionGrammarResult(input)

    val expectation: MetaObject = getFibonacciExpression
    Assert.assertEquals(expectation, result)
  }

  def getFibonacciExpression: MetaObject = {
    val condition = LessThanC.lessThan(JavaBaseModel.variable("index"), LiteralC.literal(2))
    val firstCall = JavaBaseModel.call(JavaBaseModel.variable("fibonacci"), Seq(SubtractionC.subtraction(JavaBaseModel.variable("index"), LiteralC.literal(1))))
    val secondCall = JavaBaseModel.call(JavaBaseModel.variable("fibonacci"), Seq(SubtractionC.subtraction(JavaBaseModel.variable("index"), LiteralC.literal(2))))
    val expectation = TernaryC.ternary(condition, LiteralC.literal(1), AdditionC.addition(firstCall, secondCall))
    expectation
  }

  @Test
  def testParseLessThanInsideTernary() {
    val input = "1 < 2 ? 3 : 4"
    val result: Any = getExpressionGrammarResult(input)

    val expectation: MetaObject = TernaryC.ternary(LessThanC.lessThan(LiteralC.literal(1), LiteralC.literal(2)),
      LiteralC.literal(3), LiteralC.literal(4))
    Assert.assertEquals(expectation, result)
  }

  @Test
  def testLessThan() {
    val input = "index < 2"
    val result: Any = getExpressionGrammarResult(input)
    val expectation: MetaObject = LessThanC.lessThan(JavaBaseModel.variable("index"), LiteralC.literal(2))
    Assert.assertEquals(expectation, result)
  }

  @Test
  def testAddition() {
    val input = "index + 1"
    val result: Any = getExpressionGrammarResult(input)
    val expectation: MetaObject = AdditionC.addition(JavaBaseModel.variable("index"), LiteralC.literal(1))
    Assert.assertEquals(expectation, result)
  }

  @Test
  def testSubtraction() {
    val input = "index - 1"
    val result: Any = getExpressionGrammarResult(input)
    val expectation: MetaObject = SubtractionC.subtraction(JavaBaseModel.variable("index"), LiteralC.literal(1))
    Assert.assertEquals(expectation, result)
  }

  @Test
  def testTernary() {
    val input = "1 ? 2 : 3"
    val result: Any = getExpressionGrammarResult(input)
    val expectation: MetaObject = TernaryC.ternary(LiteralC.literal(1), LiteralC.literal(2), LiteralC.literal(3))
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
    JavaMethodModel.method("main", JavaTypes.VoidType, Seq(JavaMethodModel.parameter("args", JavaTypes.arrayType(JavaTypes.stringType))),
      Seq(JavaBaseModel.call(JavaBaseModel.selector(JavaBaseModel.selector(JavaBaseModel.variable("System"), "out"), "print"),
        Seq(JavaBaseModel.call(JavaBaseModel.variable("fibonacci"), Seq(LiteralC.literal(5)))))), true, JavaMethodModel.PublicVisibility)
  }

  @Test
  def testFibonacciMethod() {
    val input = "public static int fibonacci(int index) { return index < 2 ? 1 : fibonacci(index-1) + fibonacci(index-2); }"
    val result: Any = getMethodGrammarResult(input)

    val expectation = getFibonacciMethod
    Assert.assertEquals(expectation, result)
  }

  def getFibonacciMethod: MetaObject = {
    JavaMethodModel.method("fibonacci", JavaTypes.IntType, Seq(JavaMethodModel.parameter("index", JavaTypes.IntType)),
      Seq(JavaMethodModel._return(Some(getFibonacciExpression))), static = true, JavaMethodModel.PublicVisibility)
  }

  def getMethodGrammarResult(input: String): Any = {
    val methodSelector: (Grammar) => Labelled = grammar => grammar.findGrammar(JavaBase.MethodGrammar)
    val result = getGrammarResult(input, methodSelector)
    result
  }

  @Test
  def testFibonacci() {
    val inputFile = Path("testResources") / "fibonacciWithMain" / "Fibonacci.java"

    val input = File(inputFile).slurp()

    val result = getGrammarResult(input)
    val expectation = JavaClassModel.clazz(Seq("fibonacciWithMain"), "Fibonacci", Seq(getMainMethod,getFibonacciMethod), List.empty[JavaImport])
    Assert.assertEquals(expectation, result)
  }

  def getExpressionGrammarResult(input: String): Any = {
    val grammarTransformer: (Grammar) => Grammar = grammar => grammar.findGrammar(JavaBase.ExpressionGrammar)
    val result: Any = getGrammarResult(input, grammarTransformer)
    result
  }

  def getGrammarResult(input: String, grammarTransformer: (Grammar) => Grammar = x => x): Any = {
    val parser = new TestGrammarUtils().buildParser(grammarSequence, grammarTransformer)
    val parseResult = parser(input)
    if (parseResult.isEmpty)
      Assert.fail(parseResult.toString)

    val result = parseResult.get
    Assert.assertTrue(result.toString, parseResult.next.atEnd)
    result
  }
}