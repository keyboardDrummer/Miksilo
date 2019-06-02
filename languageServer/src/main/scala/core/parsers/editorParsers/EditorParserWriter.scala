package core.parsers.editorParsers

import core.language.node.SourceRange
import core.parsers.core.{OptimizingParserWriter, ParseInput}
import langserver.types.Position

import scala.language.higherKinds

trait EditorParserWriter extends OptimizingParserWriter {

  type Input <: EditorParseInput

  trait EditorParseInput extends ParseInput {
    def position: Position
    def drop(amount: Int): Input
    def end: Input
    def printRange(end: Input): String
  }

  case class MissingInput(from: Input, to: Input, expectation: String, penalty: Double) extends ParseError[Input] {

    if (this.toString == "expected '.' but found ')'") {
      System.out.append("")
    }
    def this(from: Input, expectation: String, penalty: Double) =
      this(from, if (from.atEnd) from else from.drop(1), expectation, penalty)

    override def range: SourceRange = {
      val position = from.position
      SourceRange(position, Position(position.line, position.character + 1))
    }

    override def message: String = {
      val found = if (from.atEnd) {
        "end of source"
      } else
        from.printRange(to)

      s"expected $expectation but found '$found'"
    }
  }

  case class GenericError(location: Input, message: String, penalty: Double) extends MyParseError {
    override def append(other: MyParseError): Option[MyParseError] = None

    override def range = {
      val position = location.position
      SourceRange(position, Position(position.line, position.character + 1))
    }

    override def from = location

    override def to = from
  }

  override def succeed[Result](result: Result): Self[Result] = Succeed(result)

  case class SingleParseResult[Result](resultOption: Option[Result], errors: List[MyParseError]) {
    def successful = errors.isEmpty
    def get: Result = resultOption.get
  }

  def newSuccess[Result](result: Result, remainder: Input, score: Double): ParseResult[Result]
  case class Succeed[Result](value: Result) extends EditorParserBase[Result] with LeafParser[Result] {

    override def getParser(recursive: GetParse): Parser[Result] = {
      (input: Input, _) => newSuccess(value, input, 0)
    }

    override def getMustConsume(cache: ConsumeCache) = false
  }

  trait EditorParserBase[Result] extends ParserBuilderBase[Result] with ParserBuilder[Result] {
  }

  class MapParser[Result, NewResult](val original: Self[Result], f: Result => NewResult)
    extends EditorParserBase[NewResult] with ParserWrapper[NewResult] {

    override def getParser(recursive: GetParse): Parser[NewResult] = {
      val parseOriginal = recursive(original)

      new Parser[NewResult] {
        override def apply(input: Input, state: ParseState): ParseResult[NewResult] = parseOriginal(input, state).map(f)
      }
    }

    override def getMustConsume(cache: ConsumeCache) = cache(original)
  }

  def newFailure[Result](partial: Option[Result], input: Input, errors: MyHistory): ParseResult[Result]

  case class SuccessLog(start: Input, end: Input, value: Any) {
    override def toString = start.printRange(end)
  }

  type MyParseError = ParseError[Input]
  type MyHistory = History[Input]

  case class Fail[Result](value: Option[Result], message: String, penalty: Double)
    extends EditorParserBase[Result] with LeafParser[Result] {

    override def getParser(recursive: GetParse): Parser[Result] = {
      (input, _) => newFailure(value, input, History.error(GenericError(input, message, penalty)))
    }

    override def getMustConsume(cache: ConsumeCache) = false
  }
}
