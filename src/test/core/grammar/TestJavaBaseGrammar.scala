package core.grammar

import core.transformation._
import org.junit.{Assert, Test}
import transformations.javac.LiteralC
import transformations.javac.base._
import transformations.javac.base.model._

import scala.reflect.io.{File, Path}

class TestJavaBaseGrammar {

  @Test
  def testBasicClass() {
    val input = "package bla; class Help {}"
    val parser = new TransformationManager().buildParser(Seq(JavaBaseParse))
    val parseResult = parser(input)

    if (parseResult.isEmpty)
      Assert.fail(parseResult.toString)

    val result = parseResult.get
    val expectation = JavaClassModel.clazz(Seq("bla"), "Help", Seq.empty[MetaObject], List.empty[JavaImport])
    Assert.assertEquals(expectation, result)
  }

  @Test
  def testExpression() {

    val input = "System.out.print(fibonacci(5))"
    val parser = new TestGrammarUtils().buildParser(Seq(JavaBaseParse), grammar => grammar.findGrammar(JavaBaseParse.ExpressionGrammar))
    val parseResult = parser(input)
    if (parseResult.isEmpty)
      Assert.fail(parseResult.toString)

    Assert.assertTrue(parseResult.next.atEnd)

    val result = parseResult.get
    val expectation = JavaBaseModel.call(JavaBaseModel.selector(JavaBaseModel.selector(JavaBaseModel.variable("System"),"out"),"print"),
      Seq(JavaBaseModel.call(JavaBaseModel.variable("fibonacci"),Seq(LiteralC.literal(5)))))
    Assert.assertEquals(expectation, result)
  }

  @Test
  def testFibonacciMethod() {
    val input = "public static void main(String[] args) { System.out.print(fibonacci(5)); }"
    val parser = new TestGrammarUtils().buildParser(Seq(JavaBaseParse), grammar => grammar.findGrammar(JavaBaseParse.MethodGrammar))
    val parseResult = parser(input)
    if (parseResult.isEmpty)
      Assert.fail(parseResult.toString)

    Assert.assertTrue(parseResult.next.atEnd)

    val result = parseResult.get
    val expectation = JavaMethodModel.method("main",JavaTypes.VoidType,Seq(JavaMethodModel.parameter("args",JavaTypes.arrayType(JavaTypes.StringType))),
      Seq(JavaBaseModel.call(JavaBaseModel.selector(JavaBaseModel.selector(JavaBaseModel.variable("System"),"out"),"print"),
        Seq(JavaBaseModel.call(JavaBaseModel.variable("fibonacci"),Seq(LiteralC.literal(5)))))),true,JavaMethodModel.PublicVisibility)
    Assert.assertEquals(expectation, result)
  }

  @Test
  def testFibonacci() {
    val inputFile = Path("testResources") / "fibonacciWithMain" / "Fibonacci.java"

    val input = File(inputFile).slurp()
    val parser = new TransformationManager().buildParser(Seq(JavaBaseParse))
    val parseResult = parser(input)
    if (parseResult.isEmpty)
      Assert.fail(parseResult.toString)
    val result = parseResult.get
    val expectation = JavaClassModel.clazz(Seq("bla"), "Help", Seq.empty[MetaObject], List.empty[JavaImport])
    Assert.assertEquals(expectation, result)
  }
}