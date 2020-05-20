package miksilo.editorParser.parsers

import miksilo.editorParser.parsers.caching.ArrayOffsetManager
import miksilo.editorParser.parsers.core.ParseText
import miksilo.editorParser.parsers.editorParsers.{DelayedParseResult, History, LeftRecursiveCorrectingParserWriter, ReadyParseResult, RecursiveParseResult}
import miksilo.editorParser.parsers.strings.{CommonParserWriter, CommonStringReaderParser, StringParserWriter}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite

class LeftRecursiveCorrectingParserWriterTest extends AnyFunSuite with CommonStringReaderParser {

  test("error found in first element of left recursive series, does not trigger delays throughout the entire series") {
    lazy val absParser: Parser[Any] = new Lazy(absParser) ~ (literal("a") ~ literal("b")) | succeed("")
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

//  test("grow doesn't produce duplicates") {
//
//    val x = new DetectFixPointAndCache[Int](null)
//    val arrayOffsetManager = new ArrayOffsetManager(new ParseText("bla"), false)
//    val readyParseResult = ReadyParseResult(Some(0), arrayOffsetManager.getOffsetNode(0), (), History.empty)
//    val recursions = List(RecursiveParseResult[Unit, Int, Int](x => x.flatMap({
//      case ready: ReadyParseResult[State,Int] => singleDelayedResult(ready)
//      case delayed: DelayedParseResult[State,Int] => singleResult(delayed.map(x => x + 1))
//    }, uniform = true)))
//    val grown = x.grow(recursions, singleResult(readyParseResult))
//    assertResult(2)(grown.toList.size)
//  }
}
