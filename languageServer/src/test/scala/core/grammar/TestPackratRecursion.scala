package core.grammar

import org.junit.{Assert, Ignore, Test}

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.CharArrayReader

class TestPackratRecursion extends FunSuiteStandardTokenParsers with PackratParsers {

  test("RightRecursion") {
    lazy val parser: PackratParser[Any] = success("") ||| keyword("!") ~ parser ^^ { case a ~ b => (a, b)}
    val input = "!!!"
    lexical.delimiters += "!"
    val result = parser(new PackratReader(new lexical.Scanner(input)))

    assertResult(("!",("!",("!",""))))(result.get)
  }

  test("RightRecursion2") {
    lazy val parser: PackratParser[Any] = keyword("!") ~ parser ^^ { case a ~ b => (a, b)} ||| success("")
    val input = "!!!"
    lexical.delimiters += "!"
    val result = parser(new PackratReader(new lexical.Scanner(input)))

    assertResult(("!",("!",("!",""))))(result.get)
  }

  test("LeftRecursion") {
    lazy val parser: PackratParser[Any] = success("") ||| parser ~ keyword("!") ^^ { case a ~ b => (a,b)}
    val input = "!!!"
    lexical.delimiters += "!"
    val result = parser(new PackratReader(new lexical.Scanner(input)))

    assertResult(((("","!"),"!"),"!"))(result.get)
  }

  test("LeftRecursion2") {
    lazy val parser: PackratParser[Any] = parser ~ keyword("!") ^^ { case a ~ b => (a,b)} ||| success("")
    val input = "!!!"
    lexical.delimiters += "!"
    val result = parser(new PackratReader(new lexical.Scanner(input)))

    assertResult(((("","!"),"!"),"!"))(result.get)
  }

  test("BothRecursion") {
    lazy val parser: PackratParser[Any] = keyword("!") ||| parser ~ parser ^^ { case a ~ b => (a,b)}
    val input = "!!!"
    lexical.delimiters += "!"
    val result = parser(new PackratReader(new lexical.Scanner(input)))

    assertResult(("!",("!","!")))(result.get)
  }
}




