package core.grammar

import org.junit.{Assert, Ignore, Test}

import scala.util.parsing.combinator.syntactical.StandardTokenParsers

class TestNonPackratRecursion extends StandardTokenParsers {

  @Test
  def testRightRecursion() {
    lazy val parser: Parser[Any] = success("") ||| keyword("!") ~ parser ^^ { case a ~ b => (a, b)}
    val input = "!!!"
    lexical.delimiters += "!"
    val result = parser(new lexical.Scanner(input))

    Assert.assertEquals(("!",("!",("!",""))), result.get)
  }

  @Test
  def testRightRecursion2() {
    lazy val parser: Parser[Any] = keyword("!") ~ parser ^^ { case a ~ b => (a, b)} ||| success("")
    val input = "!!!"
    lexical.delimiters += "!"
    val result = parser(new lexical.Scanner(input))

    Assert.assertEquals(("!",("!",("!",""))), result.get)
  }

  @Ignore
  @Test
  def testLeftRecursion() {
    lazy val parser: Parser[Any] = success("") ||| parser ~ keyword("!") ^^ { case a ~ b => (a,b)}
    val input = "!!!"
    lexical.delimiters += "!"
    val result = parser(new lexical.Scanner(input))

    Assert.assertEquals(((("","!"),"!"),"!"), result.get)
  }

  @Ignore
  @Test
  def testLeftRecursion2() {
    lazy val parser: Parser[Any] = parser ~ keyword("!") ^^ { case a ~ b => (a,b)} ||| success("")
    val input = "!!!"
    lexical.delimiters += "!"
    val result = parser(new lexical.Scanner(input))

    Assert.assertEquals(((("","!"),"!"),"!"), result.get)
  }

  @Ignore
  @Test
  def testBothRecursion() {
    lazy val parser: Parser[Any] = keyword("!") ||| parser ~ parser ^^ { case a ~ b => (a,b)}
    val input = "!!!"
    lexical.delimiters += "!"
    val result = parser(new lexical.Scanner(input))

    Assert.assertEquals(("!",("!","!")), result.get)
  }
}
