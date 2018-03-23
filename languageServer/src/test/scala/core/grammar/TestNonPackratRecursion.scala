package core.grammar

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

  // This test code serves as actual documentation, to show what's not possible.
  // The test can't be turned on because it produces a stack overflow.
  //  ignore("testLeftRecursion") {
  //    lazy val parser: Parser[Any] = success("") ||| parser ~ keyword("!") ^^ { case a ~ b => (a,b)}
  //    val input = "!!!"
  //    lexical.delimiters += "!"
  //    val result = parser(new lexical.Scanner(input))
  //
  //    assertThrows[StackOverflowError](result.get)
  //  }
  //
  //  ignore("LeftRecursion2") {
  //    lazy val parser: Parser[Any] = parser ~ keyword("!") ^^ { case a ~ b => (a,b)} ||| success("")
  //    val input = "!!!"
  //    lexical.delimiters += "!"
  //    val result = parser(new lexical.Scanner(input))
  //
  //    assertResult(((("","!"),"!"),"!"))(result.get)
  //  }
  //
  //  ignore("BothRecursion") {
  //    lazy val parser: Parser[Any] = keyword("!") ||| parser ~ parser ^^ { case a ~ b => (a,b)}
  //    val input = "!!!"
  //    lexical.delimiters += "!"
  //    val result = parser(new lexical.Scanner(input))
  //
  //    assertResult(("!",("!","!")))(result.get)
  //  }
}
