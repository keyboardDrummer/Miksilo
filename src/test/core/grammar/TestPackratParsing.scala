package core.grammar

import org.junit.{Test, Assert}

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

class TestPackratParsing  extends StandardTokenParsers with PackratParsers
{
  lazy val parseTypeInner : PackratParser[TestType] =
    parseTypeInner <~ "[" <~ "]" ^^ (inner => new ArrayType(inner)) | "String" ^^ (_ => StringType)
  val parseType : PackratParser[TestType] = parseTypeInner

  lexical.reserved += "String"
  lexical.delimiters += "["
  lexical.delimiters += "]"
  lexical.delimiters ++= Seq("+","<","?",":")

  lazy val parseNumber = numericLit ^^ (n => new NumberExpression(n))
  lazy val parseLess = (parseExpression <~ "<") ~ parseExpression ^^ {case left ~ right => Less(left,right)}
  lazy val parseTernary = (parseExpression <~ "?") ~ (parseExpression <~ ":") ~ parseExpression ^^
    {case cond ~ left ~ right => Ternary(cond, left, right)}
  lazy val parseExpression : PackratParser[Expression] = parseTernary | parseLess | parseNumber

  @Test
  def testParseExpression() {

    Assert.assertEquals(new ArrayType(StringType),
      parseType(new PackratReader(new lexical.Scanner("String[]"))).get)
  }

  @Test
  def testParseType() {

    Assert.assertEquals(new Ternary(new Less(new NumberExpression(1), new NumberExpression(2)),
    NumberExpression(3),NumberExpression(4)),
      parseExpression(new PackratReader(new lexical.Scanner("1 < 2 ? 3 : 4"))).get)
  }

  trait Expression
  case class NumberExpression(value: AnyVal) extends Expression
  case class Less(first: Expression, second: Expression) extends Expression
  case class Ternary(cond: Expression, first: Expression, second: Expression) extends Expression
  trait TestType
  object StringType extends TestType
  case class ArrayType(inner: TestType) extends TestType
}
