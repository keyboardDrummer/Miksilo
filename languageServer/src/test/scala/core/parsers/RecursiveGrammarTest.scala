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
    val parseResult = head.parseWhole(new StringReader(input))
    assert(parseResult.isInstanceOf[ParseSuccess[_]])
    val result = parseResult.asInstanceOf[ParseSuccess[Any]]
    val expectation = (("a","a"),"a")
    assertResult(expectation)(result.result)
  }
}
