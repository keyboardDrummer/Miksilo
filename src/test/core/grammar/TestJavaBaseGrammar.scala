package core.grammar

import core.transformation._
import org.junit.{Assert, Test}
import transformations.javac._
import transformations.javac.base._
import transformations.javac.base.model._

import scala.reflect.io.{File, Path}
import transformations.javac.base.model.JavaImport

class TestJavaBaseGrammar {

  val grammarSequence: Seq[GrammarTransformation] =
    JavaCompiler.javaCompilerTransformations.reverse.collect( { case x: GrammarTransformation => x} )


  @Test
  def testBasicClass() {
    val input = "package bla; class Help {}"
    val parser = new TransformationManager().buildParser(grammarSequence)
    val parseResult = parser(input)

    if (parseResult.isEmpty)
      Assert.fail(parseResult.toString)

    val result = parseResult.get
    val expectation = JavaClassModel.clazz(Seq("bla"), "Help", Seq.empty[MetaObject], List.empty[JavaImport])
    Assert.assertEquals(expectation, result)
  }

  @Test
  def testMainExpression() {

    val input = "System.out.print(fibonacci(5))"
    val parser = new TestGrammarUtils().buildParser(grammarSequence, grammar => grammar.findGrammar(JavaBase.ExpressionGrammar))
    val parseResult = parser(input)
    if (parseResult.isEmpty)
      Assert.fail(parseResult.toString)

    Assert.assertTrue(parseResult.toString, parseResult.next.atEnd)

    val result = parseResult.get
    val expectation = JavaBaseModel.call(JavaBaseModel.selector(JavaBaseModel.selector(JavaBaseModel.variable("System"),"out"),"print"),
      Seq(JavaBaseModel.call(JavaBaseModel.variable("fibonacci"),Seq(LiteralC.literal(5)))))
    Assert.assertEquals(expectation, result)
  }

  @Test
  def testFibonacciExpression() {
    val input = "(index < 2) ? 1 : fibonacci(index-1) + fibonacci(index-2)"
    val parser = new TestGrammarUtils().buildParser(grammarSequence, grammar => grammar.findGrammar(JavaBase.ExpressionGrammar))
    val parseResult = parser(input)
    if (parseResult.isEmpty)
      Assert.fail(parseResult.toString)

    val result = parseResult.get
    Assert.assertTrue(result.toString, parseResult.next.atEnd)

    val condition = LessThanC.lessThan(JavaBaseModel.variable("index"), LiteralC.literal(2))
    val firstCall = JavaBaseModel.call(JavaBaseModel.variable("fibonacci"), Seq(SubtractionC.subtraction(JavaBaseModel.variable("index"), LiteralC.literal(1))))
    val secondCall = JavaBaseModel.call(JavaBaseModel.variable("fibonacci"), Seq(SubtractionC.subtraction(JavaBaseModel.variable("index"), LiteralC.literal(2))))
    val expectation = TernaryC.ternary(condition, LiteralC.literal(1), AdditionC.addition(firstCall, secondCall))
    Assert.assertEquals(expectation, result)
  }

  @Test
  def testFibonacciMainMethod() {
    val input = "public static void main(String[] args) { System.out.print(fibonacci(5)); }"
    val parser = new TestGrammarUtils().buildParser(grammarSequence, grammar => grammar.findGrammar(JavaBase.MethodGrammar))
    val parseResult = parser(input)
    if (parseResult.isEmpty)
      Assert.fail(parseResult.toString)

    Assert.assertTrue(parseResult.next.atEnd)

    val result = parseResult.get
    val expectation = JavaMethodModel.method("main",JavaTypes.VoidType,Seq(JavaMethodModel.parameter("args",JavaTypes.arrayType(JavaTypes.stringType))),
      Seq(JavaBaseModel.call(JavaBaseModel.selector(JavaBaseModel.selector(JavaBaseModel.variable("System"),"out"),"print"),
        Seq(JavaBaseModel.call(JavaBaseModel.variable("fibonacci"),Seq(LiteralC.literal(5)))))),true,JavaMethodModel.PublicVisibility)
    Assert.assertEquals(expectation, result)
  }

  @Test
  def testFibonacciMethod() {
    val input = "public static int fibonacci(int index) { return index < 2 ? 1 : fibonacci(index-1) + fibonacci(index-2); }"
    val parser = new TestGrammarUtils().buildParser(grammarSequence, grammar => grammar.findGrammar(JavaBase.MethodGrammar))
    val parseResult = parser(input)
    if (parseResult.isEmpty)
      Assert.fail(parseResult.toString)

    Assert.assertTrue(parseResult.next.atEnd)

    val result = parseResult.get
    val expectation = JavaMethodModel.method("main",JavaTypes.VoidType,Seq(JavaMethodModel.parameter("args",JavaTypes.arrayType(JavaTypes.stringType))),
      Seq(JavaBaseModel.call(JavaBaseModel.selector(JavaBaseModel.selector(JavaBaseModel.variable("System"),"out"),"print"),
        Seq(JavaBaseModel.call(JavaBaseModel.variable("fibonacci"),Seq(LiteralC.literal(5)))))),true,JavaMethodModel.PublicVisibility)
    Assert.assertEquals(expectation, result)
  }

  @Test
  def testFibonacci() {
    val inputFile = Path("testResources") / "fibonacciWithMain" / "Fibonacci.java"

    val input = File(inputFile).slurp()
    val parser = new TransformationManager().buildParser(grammarSequence)
    val parseResult = parser(input)
    if (parseResult.isEmpty)
      Assert.fail(parseResult.toString)
    val result = parseResult.get
    val expectation = JavaClassModel.clazz(Seq("bla"), "Help", Seq.empty[MetaObject], List.empty[JavaImport])
    Assert.assertEquals(expectation, result)
  }
}