package core.parsers

import org.scalatest.funsuite.AnyFunSuite
import editorParsers.AmbiguityFindingParserWriter
import editorParsers.LeftRecursiveCorrectingParserWriter
import _root_.core.parsers.editorParsers.NeverStop
import _root_.core.parsers.strings.CommonStringReaderParser

class AmbiguityFindingParserWriterTest extends AnyFunSuite
  with AmbiguityFindingParserWriter
  with CommonStringReaderParser with LeftRecursiveCorrectingParserWriter {

  test("detects trivial ambiguity") {
    val ambiguis = Literal("a") | Literal("a")
    val input = "a"
    assertThrows[Exception](ambiguis.getWholeInputParser().resetAndParse(input, NeverStop))
  }
}
