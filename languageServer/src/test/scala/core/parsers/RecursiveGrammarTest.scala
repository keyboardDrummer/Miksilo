package core.parsers

import core.parsers.strings.StringReader
import org.scalatest.FunSuite

class RecursiveGrammarTest extends FunSuite with CommonParserWriter {

  test("left recursion with lazy indirection") {
    lazy val head: Parser[Any] = new Lazy(head) ~ "a" | "a"

    val input = "aaa"
    val parseResult = head.parseWholeInput(new StringReader(input))
    assert(parseResult.isInstanceOf[ParseSuccess[_]])
    val result = parseResult.asInstanceOf[ParseSuccess[Any]]
    val expectation = (("a","a"),"a")
    assertResult(expectation)(result.result)
  }

  test("left recursion inside left recursion") {
    lazy val head: Parser[Any] = second ~ "a" | second
    lazy val second: Parser[Any] = new Lazy(second) ~ "b" | head | "c"

    val input = "caabb"
    val expectation = (((("c","a"),"a"),"b"),"b")
    val headParseResult = head.parseWholeInput(new StringReader(input))
    assert(headParseResult.isInstanceOf[ParseSuccess[_]])
    val headSuccess = headParseResult.asInstanceOf[ParseSuccess[Any]]
    assertResult(expectation)(headSuccess.result)

    val secondParseResult = second.parseWholeInput(new StringReader(input))
    assert(secondParseResult.isInstanceOf[ParseSuccess[_]])
    val secondSuccess = secondParseResult.asInstanceOf[ParseSuccess[Any]]
    assertResult(expectation)(secondSuccess.result)
  }

  val optional: Parser[Any] =  literal("a").*
  val optionalCopy: Parser[Any] = literal("a").*
  val input = "aes"
  def aesReader = new StringReader(input)

  test("Optional before seed") {
    lazy val expression: Parser[Any] = new Lazy(expression) ~ "s" | optional ~ "e"
    val result = expression.parseWholeInput(aesReader)
    assert(result.successful, result.toString)
  }

  test("Optional before choice") {
    lazy val expression: Parser[Any] = optional ~ (expression ~ "s" | "e")
    val result = expression.parseWholeInput(aesReader)
    assert(result.successful, result.toString) // This one fails in PackratParsers, not idea why. I think it succeeds for us because it detects the cycle in the '+' production since the optional has already been parsed.
  }

  test("Optional before recursive") {
    lazy val expression: Parser[Any] = optional ~ expression ~ "s" | "e"
    val result = expression.parseWholeInput(aesReader)
    assert(!result.successful, result.toString)
  }

  test("Optional before recursive and seed") {
    lazy val expression: Parser[Any] = optional ~ expression ~ "s" | optional ~ "e"
    val result = expression.parseWholeInput(aesReader)
    assert(!result.successful, result.toString) // This fails because the left-recursion in expression is not detected, because the + production starts with 'comments' which always succeeds. If we switch to allowing multiple results, then we could detect the left recursion.
  }

  test("Different optionals before recursive and seed") {
    lazy val expression: Parser[Any] = optional ~ expression ~ "s" | optionalCopy ~ "e"
    val result = expression.parseWholeInput(aesReader)
    assert(!result.successful, result.toString)
  }

  test("Ordered choice operator in the wrong order fails") {
    lazy val expression: Parser[Any] = optional ~ ("e" | expression ~ "s")
    val result = expression.parseWholeInput(aesReader)
    assert(!result.successful, result.toString)
  }

  test("Recursive defaults") {
    lazy val recursive: Parser[Any] = new Lazy(recursive) ~ "b" | "b"
    lazy val parser = "a" ~ recursive
    val input = "c"
    val expectation = ("a", ("b", "b"))
    val result = parser.parseWholeInput(new StringReader(input))
    assertResult(expectation)(result.asInstanceOf[ParseFailure[Any]].partialResult.get)
  }

  // a cycle of lazy parsers causes a stack overflow, since they have no cycle check, but with a sequence in between it just fails.
  test("only recursive with sequence indirection") {
    lazy val first: Parser[Any] = new Lazy(first) ~ "a"
    val input = "aaa"
    val parseResult = first.parseWholeInput(new StringReader(input))
    val result = parseResult.asInstanceOf[ParseFailure[Any]]
    val expectation = None
    assertResult(expectation)(result.partialResult)
  }

  test("only recursive with sequence indirection and default, " +
    "only applies the default after failing the recursion") {
    lazy val first: Parser[Any] = (new Lazy(first) ~ "a").withDefault("yes")
    val input = "aaa"
    val parseResult = first.parseWholeInput(new StringReader(input))
    val result = parseResult.asInstanceOf[ParseFailure[Any]]
    val expectation = Some("yes") //Could have been ("yes","a") with different implementation
    assertResult(expectation)(result.partialResult)
  }
}
