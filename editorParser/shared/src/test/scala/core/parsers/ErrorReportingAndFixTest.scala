package core.parsers

import org.scalatest.funsuite.AnyFunSuite
import editorParsers.LeftRecursiveCorrectingParserWriter
import editorParsers.Fix
import _root_.core.parsers.editorParsers.{Position, SourceRange, TextEdit}
import _root_.core.parsers.strings.CommonStringReaderParser

class ErrorReportingAndFixTest extends AnyFunSuite
  with CommonStringReaderParser with LeftRecursiveCorrectingParserWriter {

  test("left recursion with lazy indirection error") {
    lazy val head: Parser[Any] = new Lazy(head) ~ "#" | "!"

    val input = "@@"
    val parseResult = head.getWholeInputParser.parse(input)
    assert(!parseResult.successful)
    assertResult("expected '!'")(parseResult.errors.head.message)
  }

  test("left recursion with lazy indirection error v2") {
    lazy val head: Parser[Any] = "!" | new Lazy(head) ~ "#"

    val input = "@@"
    val parseResult = head.getWholeInputParser.parse(input)
    assert(!parseResult.successful)
    assertResult("expected '!'")(parseResult.errors.head.message)
  }

  val keywordParser = "someKeyword"

  test("Correctly fixes and reports on partial keyword") {
    val input = "someKey"
    val result = keywordParser.getWholeInputParser.parse(input)
    assert(result.errors.size == 2)
    val error = result.errors.head
    val edit = TextEdit(SourceRange(Position(0, 0), Position(0, 0)), "someKeyword")
    assertResult(Some(Fix("Insert missing symbols", edit)))(error.fix)

    val diagnosticRange = SourceRange(Position(0, 0), Position(0, 1))
    val diagnosticMessage = "expected 'someKeyword'"
    assertResult(diagnosticRange)(SourceRange(error.from.position, error.to.position))
    assertResult(diagnosticMessage)(error.message)
  }

  test("Correctly fixes and reports on missing keyword") {
    val input = ""
    val result = keywordParser.getWholeInputParser.parse(input)
    assert(result.errors.size == 1)
    val error = result.errors.head
    val edit = TextEdit(SourceRange(Position(0, 0), Position(0, 0)), "someKeyword")
    assertResult(Some(Fix("Insert missing symbols", edit)))(error.fix)

    val diagnosticRange = SourceRange(Position(0, 0), Position(0, 0))
    val diagnosticMessage = "expected 'someKeyword'"
    assertResult(diagnosticRange)(SourceRange(error.from.position, error.to.position))
    assertResult(diagnosticMessage)(error.message)
  }

  val regexInsideParenthesis = "(" ~ parseIdentifier ~ ")"
  test("Correctly fixes and reports regex ") {
    val input = ""
    val result = keywordParser.getWholeInputParser.parse(input)
    assert(result.errors.size == 1)
    val error = result.errors.head
    val edit = TextEdit(SourceRange(Position(0, 0), Position(0, 0)), "someKeyword")
    assertResult(Some(Fix("Insert missing symbols", edit)))(error.fix)

    val diagnosticRange = SourceRange(Position(0, 0), Position(0, 0))
    val diagnosticMessage = "expected 'someKeyword'"
    assertResult(diagnosticRange)(SourceRange(error.from.position, error.to.position))
    assertResult(diagnosticMessage)(error.message)
  }
}
