package core.grammar

import org.junit.{Assert, Ignore, Test}
import org.scalatest.FunSuite

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.{StandardTokenParsers, StdTokenParsers}
import scala.util.parsing.combinator.token.StdTokens

class TestNonPackratRecursion extends FunSuiteStandardTokenParsers {

  test("RightRecursion") {
    lazy val parser: Parser[Any] = success("") ||| keyword("!") ~ parser ^^ { case a ~ b => (a, b)}
    val input = "!!!"
    lexical.delimiters += "!"
    val result = parser(new lexical.Scanner(input))

    assertResult(("!",("!",("!",""))))(result.get)
  }

  test("testRightRecursion2") {
    lazy val parser: Parser[Any] = keyword("!") ~ parser ^^ { case a ~ b => (a, b)} ||| success("")
    val input = "!!!"
    lexical.delimiters += "!"
    val result = parser(new lexical.Scanner(input))

    assertResult(("!",("!",("!",""))))(result.get)
  }

  ignore("testLeftRecursion") {
    lazy val parser: Parser[Any] = success("") ||| parser ~ keyword("!") ^^ { case a ~ b => (a,b)}
    val input = "!!!"
    lexical.delimiters += "!"
    val result = parser(new lexical.Scanner(input))

    assertResult(((("","!"),"!"),"!"))(result.get)
  }

  ignore("LeftRecursion2") {
    lazy val parser: Parser[Any] = parser ~ keyword("!") ^^ { case a ~ b => (a,b)} ||| success("")
    val input = "!!!"
    lexical.delimiters += "!"
    val result = parser(new lexical.Scanner(input))

    assertResult(((("","!"),"!"),"!"))(result.get)
  }

  ignore("BothRecursion") {
    lazy val parser: Parser[Any] = keyword("!") ||| parser ~ parser ^^ { case a ~ b => (a,b)}
    val input = "!!!"
    lexical.delimiters += "!"
    val result = parser(new lexical.Scanner(input))

    assertResult(("!",("!","!")))(result.get)
  }
}
