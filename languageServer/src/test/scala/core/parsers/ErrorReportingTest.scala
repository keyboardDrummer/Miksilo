package core.parsers

import editorParsers.EditorParserWriter
import org.scalatest.FunSuite

trait ErrorReportingTest extends FunSuite with CommonStringReaderParser with EditorParserWriter  {

  test("left recursion with lazy indirection error") {
    lazy val head: EditorParser[Any] = new EditorLazy(head) ~ "c" | "a"

    val input = "bb"
    val parseResult = head.parseWholeInput(new StringReader(input))
    assert(!parseResult.successful)
    val failure = parseResult.biggestFailure.asInstanceOf[ParseFailure[_]]
    assertResult("expected 'a' but found 'b'")(failure.errors.head.message)
  }

  test("left recursion with lazy indirection error v2") {
    lazy val head: EditorParser[Any] = "a" | new EditorLazy(head) ~ "c"

    val input = "bb"
    val parseResult = head.parseWholeInput(new StringReader(input))
    assert(!parseResult.successful)
    val failure = parseResult.biggestFailure.asInstanceOf[ParseFailure[_]]
    assertResult("expected 'a' but found 'b'")(failure.errors.head.message)
  }
}
