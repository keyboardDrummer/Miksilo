package core.parsers

import _root_.core.parsers.editorParsers._
import _root_.core.parsers.editorParsers.{LeftRecursiveCorrectingParserWriter, ParseError, Position, SourceRange}
import _root_.core.parsers.strings.{CommonStringReaderParser, WhitespaceParserWriter}
import org.scalatest.FunSuite

class WhiteSpaceTest extends FunSuite {

  val parser = ExpressionParser.root.getWholeInputParser

  test("diagnostics placement in whitespace 1") {
    val program = "    "

    val result = parser.parse(new ExpressionParser.StringReader(program), UntilBestAndXStepsStopFunction())
    assertError(result.errors.head, SourceRange(Position(0,0),Position(0,4)), "expected '<expression>'")
  }

  // TODO seems to fail because of SREmpty with TODO in LeftRecursiveCorrectingParserWriter
  ignore("diagnostics placement in whitespace 2") {
    val program = "   + 3"

    val result = parser.parse(new ExpressionParser.StringReader(program))
    assertError(result.errors.head, SourceRange(Position(0,0),Position(0,3)), "expected '<expression>'")
  }

  test("diagnostics placement in whitespace 3") {
    val program = "3 +    "

    val result = parser.parse(new ExpressionParser.StringReader(program), UntilBestAndXStepsStopFunction())
    assertError(result.errors.head, SourceRange(Position(0,3),Position(0,7)), "expected '<expression>'")
  }

  test("diagnostics placement in whitespace 5") {
    val program = "let   = 3 in abc"

    val result = parser.parse(new ExpressionParser.StringReader(program), UntilBestAndXStepsStopFunction())
    assertError(result.errors.head, SourceRange(Position(0,3),Position(0,4)), "expected '<expression>'")
  }

  test("diagnostics placement in whitespace 6") {
    val program = "let abc =      in abc"

    val result = parser.parse(new ExpressionParser.StringReader(program), UntilBestAndXStepsStopFunction())
    assertError(result.errors.head, SourceRange(Position(0,9),Position(0,15)), "expected '<expression>'")
  }

  def assertError(error: ParseError[ExpressionParser.Input], range: SourceRange, message: String): Unit = {
    assertResult(range)(SourceRange(error.from.position,error.to.position))
    assertResult("expected '<expression>'")(message)
  }
}

object ExpressionParser extends CommonStringReaderParser
  with LeftRecursiveCorrectingParserWriter with WhitespaceParserWriter {

  lazy val expression: Parser[Any] = new Lazy(addition | numberParser | let | variable | hole)

  val numberParser: Parser[Any] = wholeNumber

  val addition = expression ~< "+" ~ expression

  val variable = parseIdentifier.filter(s => s != "in", x => "")
  val variableDeclaration = parseIdentifier
  val let = "let" ~> variableDeclaration ~< "=" ~ expression ~< "in" ~ expression
  val hole = Fallback(RegexParser(" *".r, "spaces"), "expression")

  val root = expression ~< trivias
}
