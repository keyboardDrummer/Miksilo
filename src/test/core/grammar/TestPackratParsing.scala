package core.grammar

import org.junit.{Test, Assert}

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

class TestPackratParsing  extends StandardTokenParsers with PackratParsers
{

  lexical.reserved += "String"
  lexical.delimiters += "["
  lexical.delimiters += "]"
  lexical.delimiters ++= Seq("+","<","?",":")


  @Test
  def testParseType() {

    lazy val parseTypeInner : PackratParser[TestType] =
      parseTypeInner <~ "[" <~ "]" ^^ (inner => new ArrayType(inner)) | "String" ^^ (_ => StringType)
    val parseType : PackratParser[TestType] = parseTypeInner

    Assert.assertEquals(new ArrayType(StringType),
      parseType(new PackratReader(new lexical.Scanner("String[]"))).get)
  }

  @Test
  def testParseOrder() {

    lazy val parseLeft = ident ^^ (_ => 2)
    lazy val parseRight = ident ^^ (_ => 1)
    lazy val parseExpression1 = parseLeft | parseRight
    lazy val parseExpression2 = parseRight | parseLeft

    Assert.assertEquals(2, parseExpression1(new PackratReader(new lexical.Scanner("a"))).get)
    Assert.assertEquals(1, parseExpression2(new PackratReader(new lexical.Scanner("a"))).get)
  }

  @Test
  def testParseExpressionPrecedenceFailure() {

    lazy val parseNumber = numericLit ^^ (n => new NumberExpression(Integer.parseInt(n)))
    lazy val parseLess = (parseExpression <~ "<") ~ parseExpression ^^ {case left ~ right => Less(left,right)}
    lazy val parseTernary = (parseExpression <~ "?") ~ (parseExpression <~ ":") ~ parseExpression ^^
      {case cond ~ left ~ right => Ternary(cond, left, right)}
    lazy val parseExpression : PackratParser[Expression] = parseTernary | parseLess | parseNumber

    Assert.assertEquals(new Less(new NumberExpression(1), new Ternary(new NumberExpression(2),
      NumberExpression(3),NumberExpression(4))),
      parseExpression(new PackratReader(new lexical.Scanner("1 < 2 ? 3 : 4"))).get)
  }

  @Test
  def testParseExpression() {

    lazy val parseNumber = numericLit ^^ (n => new NumberExpression(Integer.parseInt(n)))
    lazy val parseLess = (parseExpression1 <~ "<") ~ parseExpression1 ^^ {case left ~ right => Less(left,right)}
    lazy val parseTernary = (parseExpression2 <~ "?") ~ (parseExpression2 <~ ":") ~ parseExpression2 ^^
      {case cond ~ left ~ right => Ternary(cond, left, right)}
    lazy val parseExpression1 : PackratParser[Expression] = parseLess | parseNumber
    lazy val parseExpression2 : PackratParser[Expression] = parseTernary | parseExpression1

    Assert.assertEquals(new Ternary(new Less(new NumberExpression(1), new NumberExpression(2)),
    NumberExpression(3),NumberExpression(4)),
      parseExpression2(new PackratReader(new lexical.Scanner("1 < 2 ? 3 : 4"))).get)
  }

  trait Expression
  case class NumberExpression(value: Int) extends Expression
  case class Less(first: Expression, second: Expression) extends Expression
  case class Ternary(cond: Expression, first: Expression, second: Expression) extends Expression
  trait TestType
  object StringType extends TestType
  case class ArrayType(inner: TestType) extends TestType
}
