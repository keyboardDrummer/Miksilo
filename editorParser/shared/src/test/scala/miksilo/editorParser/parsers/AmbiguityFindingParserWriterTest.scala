package miksilo.editorParser.parsers

import org.scalatest.funsuite.AnyFunSuite
import editorParsers.{AmbiguityFindingParserWriter, LeftRecursiveCorrectingParserWriter, NeverStop}
import miksilo.editorParser.parsers.strings.{CommonParserWriter, NoStateParserWriter}

class AmbiguityFindingParserWriterTest extends AnyFunSuite
  with AmbiguityFindingParserWriter
  with NoStateParserWriter
  with CommonParserWriter with LeftRecursiveCorrectingParserWriter {

  test("detects trivial ambiguity") {
    val ambiguis = Literal("a") | Literal("a")
    val input = "a"
    assertThrows[Exception](ambiguis.getWholeInputParser().parse(input, NeverStop))
  }
}
