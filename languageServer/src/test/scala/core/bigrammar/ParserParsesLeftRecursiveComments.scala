package core.bigrammar

import core.parsers.{CommonParsers, StringReader}
import org.scalatest.FunSuite

import scala.util.matching.Regex

/*
These tests demonstrate that we can't get packrat to parse left recursive comments.
 */
class ParserParsesLeftRecursiveComments extends FunSuite with CommonParsers {

  val optional: Parser[Any] =  regex(new Regex( """/\*.*\*/""")).*
  val optionalCopy: Parser[Any] = regex(new Regex( """/\*.*\*/""")).*
  val input = "/* foo */2+3"
  def reader = new StringReader(input)

  test("direct left recursion") {
    lazy val head: Parser[Any] = new Lazy(head) ~ "a" | "a"

    val input = "aaa"
    val parseResult: ParseResult[Any] = head.parseWhole(new StringReader(input))
    assert(parseResult.isInstanceOf[ParseSuccess[_]])
    val result = parseResult.asInstanceOf[ParseSuccess[Any]]
    val expectation = (("a","a"),"a")
    assertResult(expectation)(result.result)
  }

  test("Optional before seed") {
    lazy val expression: Parser[Any] =
      new Lazy(expression) ~ literal("+") ~ expression |
        optional ~ wholeNumber
    val result = expression.parseWhole(reader)
    assert(result.successful, result.toString)
  }

  test("Optional before choice") {
    lazy val expression: Parser[Any] =
      optional ~ (expression ~ literal("+") ~ expression |
        wholeNumber)
    val result = expression.parseWhole(reader)
    assert(result.successful, result.toString) // This one fails in PackratParsers, not idea why. I think it succeeds for us because it detects the cycle in the '+' production since the optional has already been parsed.
  }

  test("Optional before recursive") {
    lazy val expression: Parser[Any] =
      optional ~ expression ~ literal("+") ~ expression |
        wholeNumber
    val result = expression.parseWhole(reader)
    assert(!result.successful, result.toString)
  }

  test("Optional before recursive and seed") {
    lazy val expression: Parser[Any] =
      optional ~ expression ~ literal("+") ~ expression |
      optional ~ wholeNumber
    val result = expression.parseWhole(reader)
    assert(!result.successful, result.toString) // This fails because the left-recursion in expression is not detected, because the + production starts with 'comments' which always succeeds. If we switch to allowing multiple results, then we could detect the left recursion.
  }

  test("Different optionals before recursive and seed") {
    lazy val expression: Parser[Any] =
      optional ~ expression ~ literal("+") ~ expression |
        optionalCopy ~ wholeNumber
    val result = expression.parseWhole(reader)
    assert(!result.successful, result.toString)
  }

//  test("Addition with comments 3") {
//    lazy val expression: Parser[Any] =
//      comments ~ (wholeNumber ||| expression ~ literal("+") ~ expression)
//    val result = expression.parseWhole(reader)
//    assert(!result.successful, result.toString)
//  }

  test("Ordered choice operator in the wrong order fails") {
    lazy val expression: Parser[Any] =
      optional ~ (wholeNumber | expression ~ literal("+") ~ expression)
    val result = expression.parseWhole(reader)
    assert(!result.successful, result.toString)
  }
}
