package core.parsers

import editorParsers.EditorParserWriter
import org.scalatest.FunSuite
import editorParsers.CorrectingParserWriter

trait ErrorReportingTest extends FunSuite with CommonStringReaderParser with CorrectingParserWriter  {

  test("left recursion with lazy indirection error") {
    lazy val head: EditorParser[Any] = new EditorLazy(head) ~ "c" | "a"

    val input = "bb"
    val parseResult = head.parseWholeInput(new StringReader(input))
    assert(!parseResult.successful)
    assertResult("expected 'a' but found 'b'")(parseResult.errors.head.message)
  }

  test("left recursion with lazy indirection error v2") {
    lazy val head: EditorParser[Any] = "a" | new EditorLazy(head) ~ "c"

    val input = "bb"
    val parseResult = head.parseWholeInput(new StringReader(input))
    assert(!parseResult.successful)
    assertResult("expected 'a' but found 'b'")(parseResult.errors.head.message)
  }
}
