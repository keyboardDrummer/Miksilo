package core.grammar

/**
  * Created by RemyW on 12-10-2016.
  */
class TestNonPackratRecursion extends StandardTokenParsers with PackratParsers {

  @Test
  def testRightRecursion() {
    lazy val parser: Parser[String] = success("") ||| keyword("!") ~ parser ^^ { case a ~ b => a + b}
    val input = "!!!!!"
    lexical.delimiters += "!"
    val result = parser(new lexical.Scanner(input))

    Assert.assertEquals(input, result.get)
  }

  @Test
  def testRightRecursion2() {
    lazy val parser: Parser[String] = keyword("!") ~ parser ^^ { case a ~ b => a + b} ||| success("")
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

  @Ignore
  @Test
  def testLeftRecursion2() {
    lazy val parser: Parser[String] = parser ~ keyword("!") ^^ { case a ~ b => a + b} | success("")
    val input = "!!!!!"
    lexical.delimiters += "!"
    val result = parser(new lexical.Scanner(input))

    Assert.assertEquals(input, result.get)
  }
}
