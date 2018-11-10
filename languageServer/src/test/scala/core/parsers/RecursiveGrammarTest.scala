package core.parsers

import org.scalatest.FunSuite

class RecursiveGrammarTest extends FunSuite with CommonParsers {

  test("left recursion with lazy indirection") {
    lazy val head: Parser[Any] = new Lazy(head) ~ "a" | "a"

    val input = "aaa"
    val parseResult = head.parseWhole(new StringReader(input))
    assert(parseResult.isInstanceOf[ParseSuccess[_]])
    val result = parseResult.asInstanceOf[ParseSuccess[Any]]
    val expectation = (("a","a"),"a")
    assertResult(expectation)(result.result)
  }

  test("left recursion inside left recursion") {
    lazy val head: Parser[Any] = second ~ "a" | second
    lazy val second: Parser[Any] = new Lazy(second) ~ "b" | head | "c"

    val input = "caabb"
    val headParseResult = head.parseWhole(new StringReader(input))
    assert(headParseResult.isInstanceOf[ParseSuccess[_]])
    val headSuccess = headParseResult.asInstanceOf[ParseSuccess[Any]]
    val expectation = (((("c","a"),"a"),"b"),"b")
    assertResult(expectation)(headSuccess.result)

    val secondParseResult = second.parseWhole(new StringReader(input))
    assert(secondParseResult.isInstanceOf[ParseSuccess[_]])
    val secondSuccess = secondParseResult.asInstanceOf[ParseSuccess[Any]]
    assertResult(expectation)(secondSuccess.result)
  }
}
