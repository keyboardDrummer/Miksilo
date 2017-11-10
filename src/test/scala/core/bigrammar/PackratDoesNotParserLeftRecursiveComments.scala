package core.bigrammar

import org.scalatest.FunSuite

import scala.util.matching.Regex
import scala.util.parsing.combinator.{JavaTokenParsers, PackratParsers}
import scala.util.parsing.input.CharArrayReader

/*
These tests demonstrate that we can't get packrat to parse left recursive comments.
 */
class PackratDoesNotParserLeftRecursiveComments extends FunSuite with JavaTokenParsers with PackratParsers {

  val comments: PackratParser[Any] = regex(new Regex( """/\*.*\*/""")).*
  val comments2: PackratParser[Any] = regex(new Regex( """/\*.*\*/""")).*
  val input = "/* foo */ 2 + 3"
  def reader = new PackratReader[Char](new CharArrayReader(input.toCharArray))

  test("Addition with comments") {
    lazy val expression: PackratParser[Any] =
      comments ~ expression ~ literal("+") ~ expression |
      comments ~ wholeNumber
    val result = phrase(expression)(reader)
    assert(!result.successful, result.toString)
  }

  test("Addition with comments 2") {
    lazy val expression: PackratParser[Any] =
      comments ~ (expression ~ literal("+") ~ expression |
        wholeNumber)
    val result = phrase(expression)(reader)
    assert(!result.successful, result.toString)
  }

  test("Addition with comments 3") {
    lazy val expression: PackratParser[Any] =
      comments ~ (wholeNumber ||| expression ~ literal("+") ~ expression)
    val result = phrase(expression)(reader)
    assert(!result.successful, result.toString)
  }

  test("Addition with comments 4") {
    lazy val expression: PackratParser[Any] =
      comments ~ (wholeNumber | expression ~ literal("+") ~ expression)
    val result = phrase(expression)(reader)
    assert(!result.successful, result.toString)
  }

  test("Addition with comments 5") {
    lazy val expression: PackratParser[Any] =
      comments ~ (wholeNumber | expression ~ literal("+") ~ expression)
    val result = phrase(expression)(reader)
    assert(!result.successful, result.toString)
  }

  test("Addition with comments 1.5") {
    lazy val expression: PackratParser[Any] =
      comments ~ expression ~ literal("+") ~ expression |
        comments2 ~ wholeNumber
    val result = phrase(expression)(reader)
    assert(!result.successful, result.toString)
  }

  test("Addition with comments 1.2") {
    lazy val expression: PackratParser[Any] =
      comments ~ expression ~ literal("+") ~ expression |
        wholeNumber
    val result = phrase(expression)(reader)
    assert(!result.successful, result.toString)
  }

  test("Addition with comments 1.3") {
    lazy val expression: PackratParser[Any] =
      expression ~ literal("+") ~ expression |
        comments ~ wholeNumber
    val result = phrase(expression)(reader)
    assert(!result.successful, result.toString)
  }
}
