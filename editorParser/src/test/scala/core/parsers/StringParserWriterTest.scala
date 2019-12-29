package core.parsers

import org.scalatest.funsuite.AnyFunSuite
import editorParsers.LeftRecursiveCorrectingParserWriter
import _root_.core.parsers.strings.CommonStringReaderParser

class StringParserWriterTest extends AnyFunSuite
  with CommonStringReaderParser with LeftRecursiveCorrectingParserWriter {

  test("keywordParser cannot parse start of identifier") {
    val parser = "keyword" ~> parseIdentifier
    val input = "keywordAsIdentifierPrefix"
    val result = parser.getWholeInputParser.parse(new StringReader(input))
    assertResult("keywordAsIdentifierPrefix")(result.resultOption.get)
    assertResult("expected 'keyword'")(result.errors.head.message)
  }

  test("keyword errors merge correctly") {
    val parser = "keyword" ~> "secondKeyword" ~> parseIdentifier
    val input = "keywordAsIdentifierPrefix"
    val result = parser.getWholeInputParser.parse(new StringReader(input))
    assert(result.errors.size == 1)
    assertResult("keywordAsIdentifierPrefix")(result.resultOption.get)
    assertResult("expected 'keywordsecondKeyword'")(result.errors.head.message)
  }
}
