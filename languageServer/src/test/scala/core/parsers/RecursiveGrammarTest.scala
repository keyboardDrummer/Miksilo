package core.parsers

import org.scalatest.FunSuite

class RecursiveGrammarTest extends FunSuite with CommonParsers {
  test("direct left recursion") {
    lazy val head: Parser[Any] = new Lazy(head) ~ "a" | "a"

    val input = "aaa"
    val parseResult = head.parseWhole(new StringReader(input))
    assert(parseResult.isInstanceOf[ParseSuccess[_]])
    val result = parseResult.asInstanceOf[ParseSuccess[Any]]
    val expectation = (("a","a"),"a")
    assertResult(expectation)(result.result)
  }
}
