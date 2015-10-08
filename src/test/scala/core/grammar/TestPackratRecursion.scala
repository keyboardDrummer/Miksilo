package core.grammar

import org.junit.{Assert, Ignore, Test}

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

class TestPackratRecursion extends StandardTokenParsers with PackratParsers {

  @Test
  def testRightRecursion() {
    lazy val parser: Parser[String] = success("") ||| keyword("!") ~ parser ^^ { case a ~ b => a + b}
    val input = "!!!!!"
    lexical.delimiters += "!"
    val result = parser(new lexical.Scanner(input))

    Assert.assertEquals(input, result.get)
  }

  @Ignore
  @Test
  def testLeftRecursion() {
    lazy val parser: Parser[String] = success("") ||| parser ~ keyword("!") ^^ { case a ~ b => a + b}
    val input = "!!!!!"
    lexical.delimiters += "!"
    val result = parser(new lexical.Scanner(input))

    Assert.assertEquals(input, result.get)
  }
}
