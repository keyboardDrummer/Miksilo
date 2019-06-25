package core.parsers

import org.scalatest.FunSuite
import editorParsers.LeftRecursiveCorrectingParserWriter

class ErrorReportingTest extends FunSuite with CommonStringReaderParser with LeftRecursiveCorrectingParserWriter  {

  test("left recursion with lazy indirection error") {
    lazy val head: Self[Any] = new Lazy(head) ~ "#" | "!"

    val input = "@@"
    val parseResult = head.getWholeInputParser.parse(new StringReader(input))
    assert(!parseResult.successful)
    assertResult("expected '!'")(parseResult.errors.last.message)
  }

  test("left recursion with lazy indirection error v2") {
    lazy val head: Self[Any] = "!" | new Lazy(head) ~ "#"

    val input = "@@"
    val parseResult = head.getWholeInputParser.parse(new StringReader(input))
    assert(!parseResult.successful)
    assertResult("expected '!'")(parseResult.errors.last.message)
  }
}
