package core.grammar

import org.junit.{Assert, Test}
import org.scalatest.FunSuite

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.{StandardTokenParsers, StdTokenParsers}
import scala.util.parsing.combinator.token.StdTokens

class FunSuiteStandardTokenParsers extends FunSuite with StdTokenParsers {
  type Tokens = StdTokens
  val lexical = new StdLexical

  //an implicit keyword function that gives a warning when a given word is not in the reserved/delimiters list
  override implicit def keyword(chars : String): Parser[String] =
  if(lexical.reserved.contains(chars) || lexical.delimiters.contains(chars)) super.keyword(chars)
  else failure("You are trying to parse \""+chars+"\", but it is neither contained in the delimiters list, nor in the reserved keyword list of your lexical object")
}

class TestPackratParsing extends FunSuiteStandardTokenParsers with PackratParsers {

  lexical.reserved += "String"
  lexical.delimiters += "["
  lexical.delimiters += "]"
  lexical.delimiters ++= Seq("+", "<", "?", ":")


  test("ParseType") {

    lazy val parseTypeInner: PackratParser[TestType] =
      parseTypeInner <~ "[" <~ "]" ^^ (inner => new ArrayType(inner)) | "String" ^^ (_ => StringType)
    val parseType: PackratParser[TestType] = parseTypeInner

    assertResult(new ArrayType(StringType))(parseType(new PackratReader(new lexical.Scanner("String[]"))).get)
  }

  test("ParseOrder") {

    lazy val parseLeft = ident ^^ (_ => 2)
    lazy val parseRight = ident ^^ (_ => 1)
    lazy val parseExpression1 = parseLeft | parseRight
    lazy val parseExpression2 = parseRight | parseLeft

    assertResult(2)(parseExpression1(new PackratReader(new lexical.Scanner("a"))).get)
    assertResult(1)(parseExpression2(new PackratReader(new lexical.Scanner("a"))).get)
  }


  test("ParseExpressionPrecedenceFailure") {

    lazy val parseNumber = numericLit ^^ (n => new NumberExpression(Integer.parseInt(n)))
    lazy val parseLess = (parseExpression <~ "<") ~ parseExpression ^^ { case left ~ right => Less(left, right)}
    lazy val parseTernary = (parseExpression <~ "?") ~ (parseExpression <~ ":") ~ parseExpression ^^ { case cond ~ left ~ right => Ternary(cond, left, right)}
    lazy val parseExpression: PackratParser[Expression] = parseTernary | parseLess | parseNumber

    assertResult(new Less(new NumberExpression(1), new Ternary(new NumberExpression(2),
      NumberExpression(3), NumberExpression(4))))(
      parseExpression(new PackratReader(new lexical.Scanner("1 < 2 ? 3 : 4"))).get)
  }

  test("ParseExpression") {

    lazy val parseNumber = numericLit ^^ (n => new NumberExpression(Integer.parseInt(n)))
    lazy val parseLess = (parseExpression1 <~ "<") ~ parseExpression1 ^^ { case left ~ right => Less(left, right)}
    lazy val parseTernary = (parseExpression2 <~ "?") ~ (parseExpression2 <~ ":") ~ parseExpression2 ^^ { case cond ~ left ~ right => Ternary(cond, left, right)}
    lazy val parseExpression1: PackratParser[Expression] = parseLess | parseNumber
    lazy val parseExpression2: PackratParser[Expression] = parseTernary | parseExpression1

    assertResult(new Ternary(new Less(new NumberExpression(1), new NumberExpression(2)),
      NumberExpression(3), NumberExpression(4)))(
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
