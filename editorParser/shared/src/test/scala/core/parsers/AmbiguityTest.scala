package core.parsers

import org.scalatest.funsuite.AnyFunSuite
import editorParsers.LeftRecursiveCorrectingParserWriter
import _root_.core.parsers.strings.CommonStringReaderParser

class AmbiguityTest extends AnyFunSuite with CommonStringReaderParser with LeftRecursiveCorrectingParserWriter {

  val optional_a: SequenceParserExtensions[Any] =  Literal("!").*
  val optionalCopy: SequenceParserExtensions[Any] = Literal("!").*
  def aesReader = "!#@"

  test("Basic ambiguity test") {
    lazy val expression: Parser[Any] = ("!@" | "!") ~ "@#"
    val result = expression.getWholeInputParser().parse("!@#")
    assert(result.successful, result.toString)
  }

  test("Optional before choice") {
    lazy val expression: Parser[Any] = optional_a ~ (expression ~ "@" | "#")
    val result = expression.getWholeInputParser().parse(aesReader)
    assert(result.successful, result.toString)
  }

  test("Optional before recursive and seed") {
    lazy val expression: Parser[Any] = optional_a ~ expression ~ "@" | optional_a ~ "#"
    val result = expression.getWholeInputParser().parse(aesReader)
    assert(result.successful, result.toString) // This fails because the left-recursion in expression is not detected, because the + production starts with 'comments' which always succeeds. If we switch to allowing multiple results, then we could detect the left recursion.
  }

  test("Different optionals before recursive and seed") {
    lazy val expression: Parser[Any] = optional_a ~ expression ~ "@" | optionalCopy ~ "#"
    val result = expression.getWholeInputParser().parse(aesReader)
    assert(result.successful, result.toString)
  }

  test("Ordered choice operator in the wrong order") {
    lazy val expression: Parser[Any] = optional_a ~ choice("#", expression ~ "@")
    val result = expression.getWholeInputParser().parse(aesReader)
    assert(result.successful, result.toString)
  }
}
