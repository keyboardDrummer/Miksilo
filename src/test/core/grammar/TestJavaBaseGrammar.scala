package core.grammar

import org.junit.{Assert, Test}
import core.transformation._
import transformations.javac.base._
import scala.reflect.io.{Path, File}
import transformations.javac.base.model.{JavaTypes, JavaClassModel}
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.collection.mutable
import transformations.javac.base.model.JavaImport

class TestJavaBaseGrammar {

  @Test
  def testBasicClass {
    val input = "package bla; class Help {}"
    val parser = TransformationManager.buildParser(Seq(JavaBaseParse))
    val result = parser(input).get
    val expectation = JavaClassModel.clazz(Seq("bla"), "Help", Seq.empty[MetaObject], List.empty[JavaImport])
    Assert.assertEquals(expectation, result)
  }


  @Test
  def testExpression {

    val input = "System.out.print(fibonacci(5))"
    val parser = TransformationManager.buildParser(Seq(JavaBaseParse))
    val parseResult = parser(input)
    if (parseResult.isEmpty)
      Assert.fail(parseResult.toString)

    val remainingInput = parseResult.next
    val result = parseResult.get
    val expectation = JavaClassModel.clazz(Seq("bla"), "Help", Seq.empty[MetaObject], List.empty[JavaImport])
    Assert.assertEquals(expectation, result)
  }

  @Test
  def testFibonacci {
    val inputFile = Path("src") / "transformations" / "javac" / "testing" / "fibonacciWithMain" / "Fibonacci.java"

    val input = File(inputFile).slurp()
    val parser = TransformationManager.buildParser(Seq(JavaBaseParse))
    val parseResult = parser(input)
    if (parseResult.isEmpty)
      Assert.fail(parseResult.toString)
    val result = parseResult.get
    val expectation = JavaClassModel.clazz(Seq("bla"), "Help", Seq.empty[MetaObject], List.empty[JavaImport])
    Assert.assertEquals(expectation, result)
  }

  object RatPacker extends StandardTokenParsers with PackratParsers
  {
    lazy val parseTypeInner : PackratParser[TestType] =
      parseTypeInner <~ "[" <~ "]" ^^ (inner => new ArrayType(inner)) | "String" ^^ (_ => StringType)
    val parseType : PackratParser[TestType] = parseTypeInner

    lexical.reserved += "String"
    lexical.delimiters += "["
    lexical.delimiters += "]"

    def testPackrat() {

      Assert.assertEquals(new ArrayType(StringType),
        parseType(new PackratReader(new lexical.Scanner("String[]"))).get)
    }
  }

  @Test
  def testPackrat() {
    RatPacker.testPackrat()
  }

  object TestGrammar extends GrammarTransformation
  {
    override def transformDelimiters(delimiters: mutable.HashSet[String]): Unit = delimiters ++= Seq("[","]")

    override def transformGrammar(grammar: Grammar): Grammar = {

      lazy val parseString = "String" ^^ (_ => JavaTypes.StringType)
      lazy val parseType2 : Grammar = new Lazy(parseType2) <~ "[" <~ "]" ^^
        ( _type => JavaTypes.arrayType(_type)) | parseString
      val identity : Any => Any = x => x
      lazy val parseType : Grammar = parseString ~ (("[" ~> "]" ^^ (_ => (s: Any) => JavaTypes.arrayType(s))) | produce(identity)) ^^
        { case s seqr f => f.asInstanceOf[Any => Any](s) }

      // { case _type seqr _ => JavaTypes.arrayType(_type)} | parseString
      parseType2
    }

    override def transformReserved(reserved: mutable.HashSet[String]): Unit = reserved += "String"

    override def transform(program: MetaObject, state: TransformationState): Unit = ???

    override def dependencies: Set[ProgramTransformation] = ???
  }

  @Test
  def testGrammar() {

    val input = "String[]"
    val parser = TransformationManager.buildParser(Seq(TestGrammar))
    val result = parser(input).get
    Assert.assertEquals(JavaTypes.arrayType(JavaTypes.StringType), result)
  }

  trait TestType
  object StringType extends TestType
  case class ArrayType(inner: TestType) extends TestType
}