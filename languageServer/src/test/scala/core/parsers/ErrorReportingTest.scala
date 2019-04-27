package core.parsers

import org.scalatest.FunSuite
import editorParsers.LeftRecursiveCorrectingParserWriter

class ErrorReportingTest extends FunSuite with CommonStringReaderParser with LeftRecursiveCorrectingParserWriter  {

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
