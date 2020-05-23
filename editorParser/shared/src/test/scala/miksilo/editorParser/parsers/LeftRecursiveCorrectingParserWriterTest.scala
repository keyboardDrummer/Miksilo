package miksilo.editorParser.parsers

import miksilo.editorParser.parsers.caching.ArrayOffsetManager
import miksilo.editorParser.parsers.core.ParseText
import miksilo.editorParser.parsers.editorParsers.{DelayedParseResult, History, LeftRecursiveCorrectingParserWriter, ReadyParseResult, RecursiveParseResult}
import miksilo.editorParser.parsers.strings.{CommonParserWriter, CommonStringReaderParser, StringParserWriter}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite

class LeftRecursiveCorrectingParserWriterTest extends AnyFunSuite {

  test("error found in first element of left recursive series, does not trigger delays throughout the entire series") {
    import CommonStringReaderParser._
    lazy val absParser: Parser[Any] = new Lazy(absParser) ~ (literal("a") ~ literal("b")) | CommonStringReaderParser.succeed("")
    val parser = ParseWholeInput(absParser)
    val program = "aababab"
    val arrayOffsetManager = new ArrayOffsetManager(new ParseText(program), false)
    val builtParser = compile(parser).buildParser(parser)
    val zero = arrayOffsetManager.getOffsetNode(0)
    val results = builtParser.parser.apply(zero, (), newParseState(zero))
    val secondResults = results.toList.head.asInstanceOf[DelayedParseResult[_, _]].getResults
    assert(secondResults.toList.head.isInstanceOf[ReadyParseResult[_, _]])
  }

  test("delayed returns only returns another delayed result if it has more errors") {
    import CommonStringReaderParser._
    val abcParser = literal("a") ~ literal("b") ~ literal("c")
    val parser = ParseWholeInput(abcParser)
    val program = "bc"
    val arrayOffsetManager = new ArrayOffsetManager(new ParseText(program), false)
    val builtParser = compile(parser).buildParser(parser)
    val zero = arrayOffsetManager.getOffsetNode(0)
    val results = builtParser.parser.apply(zero, (), newParseState(zero))
    val secondResults = results.toList.head.asInstanceOf[DelayedParseResult[_, _]].getResults
    assert(secondResults.toList.head.isInstanceOf[ReadyParseResult[_, _]])
  }

  /*
  This test was added to verify that a bug in grow has been fixed that merges the ungrown and growing result before growning the latter,
  which for some reason causes problems later down the line.
   */
  test("left recursive grammars properly prioritize") {
    object ParserWriter extends CommonStringReaderParser {
      override val maxListDepth: Int = 2
    }
    import ParserWriter._
    lazy val absParser: Parser[Any] = new Lazy(absParser) ~ (literal("a") ~ (literal("b") | literal("c"))) | ParserWriter.succeed("")
    val parser = absParser.getWholeInputParser()
    val program = "aababab"

    // Ik moet twee equivalent delayed parse results hebben, waarbij de tweede de juiste is, maar de eerste 2 resultaten genereert die tijdelijk beter scoren dan de tweede,
    // Het tijdelijk beter scoren kan I guess door gewoon te stoppen met input parsen.
    val result = parser.parse(program)
    assert(result.errors.size == 1)
    val error = result.errors.head
    assert(error.from.offset == 1 && error.to.offset == 2)
  }
}
