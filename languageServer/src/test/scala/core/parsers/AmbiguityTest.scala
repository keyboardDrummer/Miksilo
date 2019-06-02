package core.parsers

import org.scalatest.FunSuite
import editorParsers.LeftRecursiveCorrectingParserWriter

class AmbiguityTest extends FunSuite with CommonStringReaderParser with LeftRecursiveCorrectingParserWriter {

  val optional_a: EditorParserExtensions[Any] =  Literal("!").*
  val optionalCopy: EditorParserExtensions[Any] = Literal("!").*
  def aesReader = new StringReader("!#@")

  test("Basic ambiguity test") {
    lazy val expression: Self[Any] = ("!@" | "!") ~ "@#"
    val result = expression.getWholeInputParser()(new StringReader("!@#"))
    assert(result.successful, result.toString)
  }

  test("Optional before choice") {
    lazy val expression: Self[Any] = optional_a ~ (expression ~ "@" | "#")
    val result = expression.getWholeInputParser()(aesReader)
    assert(result.successful, result.toString)
  }

  test("Optional before recursive and seed") {
    lazy val expression: Self[Any] = optional_a ~ expression ~ "@" | optional_a ~ "#"
    val result = expression.getWholeInputParser()(aesReader)
    assert(result.successful, result.toString) // This fails because the left-recursion in expression is not detected, because the + production starts with 'comments' which always succeeds. If we switch to allowing multiple results, then we could detect the left recursion.
  }

  test("Different optionals before recursive and seed") {
    lazy val expression: Self[Any] = optional_a ~ expression ~ "@" | optionalCopy ~ "#"
    val result = expression.getWholeInputParser()(aesReader)
    assert(result.successful, result.toString)
  }

  test("Ordered choice operator in the wrong order") {
    lazy val expression: Self[Any] = optional_a ~ choice("#", expression ~ "@")
    val result = expression.getWholeInputParser()(aesReader)
    assert(result.successful, result.toString)
  }
}
