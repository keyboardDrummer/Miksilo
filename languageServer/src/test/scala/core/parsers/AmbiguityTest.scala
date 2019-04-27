package core.parsers

import org.scalatest.FunSuite
import editorParsers.CorrectingParserWriter

class AmbiguityTest extends FunSuite with CommonStringReaderParser with CorrectingParserWriter {

  val optional_a: EditorParserExtensions[Any] =  literal("a").*
  val optionalCopy: EditorParserExtensions[Any] = literal("a").*
  def aesReader = new StringReader("aes")

  test("Different optionals before recursive and seed FAILS") {
    lazy val expression: EditorParser[Any] = optional_a ~ expression ~ "s" | optionalCopy ~ "e"
    val result = expression.parseWholeInput(aesReader)
    assert(!result.successful, result.toString)
  }

  test("Ordered choice operator in the wrong order FAILS") {
    lazy val expression: EditorParser[Any] = optional_a ~ choice("e", expression ~ "s", true)
    val result = expression.parseWholeInput(aesReader)
    assert(!result.successful, result.toString)
  }

  test("Optional before recursive and seed FAILS") {
    lazy val expression: EditorParser[Any] = optional_a ~ expression ~ "s" | optional_a ~ "e"
    val result = expression.parseWholeInput(aesReader)
    assert(!result.successful, result.toString) // This fails because the left-recursion in expression is not detected, because the + production starts with 'comments' which always succeeds. If we switch to allowing multiple results, then we could detect the left recursion.
  }

  test("Optional before choice") {
    lazy val expression: EditorParser[Any] = optional_a ~ (expression ~ "s" | "e")
    val result = expression.parseWholeInput(aesReader)
    assert(!result.successful, result.toString) // This one fails in unambiguous parsers
  }

  test("Basic ambiguity test fails") {
    lazy val expression: EditorParser[Any] = ("ab" | "a") ~ "bc"
    val result = expression.parseWholeInput(new StringReader("abc"))
    assert(!result.successful, result.toString)
  }

  test("Basic ambiguity test") {
    lazy val expression: EditorParser[Any] = ("ab" | "a") ~ "bc"
    val result = expression.parseWholeInput(new StringReader("abc"))
    assert(result.successful, result.toString)
  }

  test("Optional before choice") {
    lazy val expression: EditorParser[Any] = optional_a ~ (expression ~ "s" | "e")
    val result = expression.parseWholeInput(aesReader)
    assert(result.successful, result.toString) // This one fails in unambiguous parsers
  }

  test("Optional before recursive and seed") {
    lazy val expression: EditorParser[Any] = optional_a ~ expression ~ "s" | optional_a ~ "e"
    val result = expression.parseWholeInput(aesReader)
    assert(result.successful, result.toString) // This fails because the left-recursion in expression is not detected, because the + production starts with 'comments' which always succeeds. If we switch to allowing multiple results, then we could detect the left recursion.
  }

  test("Different optionals before recursive and seed") {
    lazy val expression: EditorParser[Any] = optional_a ~ expression ~ "s" | optionalCopy ~ "e"
    val result = expression.parseWholeInput(aesReader)
    assert(result.successful, result.toString)
  }

  test("Ordered choice operator in the wrong order") {
    lazy val expression: EditorParser[Any] = optional_a ~ choice("e", expression ~ "s", leftIsAlwaysBigger = true)
    val result = expression.parseWholeInput(aesReader)
    assert(result.successful, result.toString)
  }

}
