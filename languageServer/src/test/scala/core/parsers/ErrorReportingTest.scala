package core.parsers

import org.scalatest.FunSuite
import editorParsers.LeftRecursiveCorrectingParserWriter

class ErrorReportingTest extends FunSuite with CommonStringReaderParser with LeftRecursiveCorrectingParserWriter  {

  test("left recursion with lazy indirection error") {
    lazy val head: Self[Any] = new Lazy(head) ~ "c" | "a"

    val input = "bb"
    val parseResult = head.parseWholeInput(new StringReader(input))
    assert(!parseResult.successful)
    assertResult("expected 'a' but found 'b'")(parseResult.errors.last.message)
  }

  test("left recursion with lazy indirection error v2") {
    lazy val head: Self[Any] = "a" | new Lazy(head) ~ "c"

    val input = "bb"
    val parseResult = head.parseWholeInput(new StringReader(input))
    assert(!parseResult.successful)
    assertResult("expected 'a' but found 'b'")(parseResult.errors.last.message)
  }
}
