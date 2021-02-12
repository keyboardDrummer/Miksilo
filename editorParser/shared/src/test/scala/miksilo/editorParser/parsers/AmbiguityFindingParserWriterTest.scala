package miksilo.editorParser.parsers

import org.scalatest.funsuite.AnyFunSuite
import editorParsers.{AmbiguityFindingParserWriter, LeftRecursiveCorrectingParserWriter, NeverStop}
import miksilo.editorParser.parsers.strings.{CommonParserWriter, NoStateParserWriter}

class AmbiguityFindingParserWriterTest extends AnyFunSuite
  with AmbiguityFindingParserWriter
  with NoStateParserWriter
  with CommonParserWriter with LeftRecursiveCorrectingParserWriter {

  ignore("detects trivial ambiguity") {
    val ambiguous = Literal("aa") | Literal("a")
    val input = "aa"
    assertThrows[Exception](ambiguous.getWholeInputParser().parse(input, NeverStop))
  }
}
