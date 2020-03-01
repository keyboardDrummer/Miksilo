package core.bigrammar

import core.bigrammar.BiGrammarToParser.{Input, Reader, toParserBuilder}
import core.bigrammar.printer.BiGrammarToPrinter
import core.parsers.editorParsers.{SingleParseResult, UntilBestAndXStepsStopFunction}
import org.scalatest.funsuite.AnyFunSuite

object TestGrammarUtils extends AnyFunSuite {

  def parseAndPrintSame(example: String, expectedOption: Option[Any] = None, grammarDocument: BiGrammar): Unit = {
    val documentResult: String = parseAndPrint(example, expectedOption, grammarDocument)
    assertResult(example)(documentResult)
  }

  def parseAndPrint(example: String, expectedOption: Option[Any], grammarDocument: BiGrammar): String = {
    val parseResult = parse(example, grammarDocument)
    assert(parseResult.successful, parseResult.toString)

    val result = parseResult.get

    expectedOption.foreach(expected => assertResult(expected)(result))

    print(result, grammarDocument)
  }

  def print(result: Any, grammarDocument: BiGrammar): String = {
    BiGrammarToPrinter.toDocument(result, grammarDocument).renderString()
  }

  def parse(example: String, grammarDocument: BiGrammar): SingleParseResult[Any, Input] = {
    val parser = toParserBuilder(grammarDocument)
    parser.getWholeInputParser().resetAndParse(example, UntilBestAndXStepsStopFunction())
  }
}
