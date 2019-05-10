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

  case class MissingInput(from: Input, to: Input, expectation: String, penalty: Double) extends ParseError {
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

      s"expected $expectation but found $found"
    }
  }

  case class GenericError(location: Input, message: String, penalty: Double) extends ParseError {
    override def append(other: ParseError): Option[ParseError] = None

    override def range = {
      val position = location.position
      SourceRange(position, Position(position.line, position.character + 1))
    }

    override def from = location

    override def to = from
  }

  type Self[+Result] = LRParser[Result]

  override def succeed[Result](result: Result): Self[Result] = Succeed(result)

  case class ParseWholeResult[Result](resultOption: Option[Result], errors: List[ParseError]) {
    def successful = errors.isEmpty
    def get: Result = resultOption.get
  }

  case class Succeed[Result](value: Result) extends EditorParserBase[Result] with LeafParser[Result] {

    override def getParser(recursive: GetParse): Parse[Result] = {
      (input: Input, _) => newSuccess(value, input)
    }

    override def getMustConsume(cache: ConsumeCache) = false
  }

  trait EditorParserBase[Result] extends ParserBase[Result] with LRParser[Result] {
  }

  class MapParser[Result, NewResult](val original: Self[Result], f: Result => NewResult)
    extends EditorParserBase[NewResult] with ParserWrapper[NewResult] {

    override def getParser(recursive: GetParse): Parse[NewResult] = {
      val parseOriginal = recursive(original)

      new Parse[NewResult] {
        override def apply(input: Input, state: ParseState): ParseResult[NewResult] = parseOriginal(input, state).map(f)
      }
    }

    override def getMustConsume(cache: ConsumeCache) = cache(original)
  }

  def newFailure[Result](partial: Option[Result], input: Input, errors: History): ParseResult[Result]

  object PositionParser extends EditorParserBase[Input] with LeafParser[Input] {

    override def getParser(recursive: GetParse): Parse[Input] = {
      (input, _) => newSuccess(input, input)
    }

    override def getMustConsume(cache: ConsumeCache) = false
  }

  case class SuccessLog(start: Input, end: Input, value: Any) {
    override def toString = start.printRange(end)
  }

  case class History(score: Double, errors: List[ParseError], successes: List[SuccessLog]) {
    def this() = this(0, List.empty, List.empty)
    def this(error: ParseError) = this(error.score, List(error), List.empty)

    if (this.hashCode() == 642065833) {
      System.out.append("")
    }

    def ++(right: History): History = {
      if (errors.isEmpty)
        return History(score + right.score, right.errors, successes ++ right.successes)

      val (withoutLast, last :: Nil) = errors.splitAt(errors.length - 1)
      val newLeft = History(score - last.score, withoutLast, List.empty)
      val newRight = right.addError(last)
      History(newLeft.score + newRight.score, newLeft.errors ++ newRight.errors, successes ++ right.successes)
    }

    def addTestSuccess(point: Input): History = {
      History(score + HistoryConstants.successValue, errors, successes)
    }

    def addSuccess(start: Input, end: Input, value: Any, successScore: Double = HistoryConstants.successValue): History = {
      History(score + successScore, errors, SuccessLog(start, end, value) :: successes)
    }

    def addError(newHead: ParseError): History = {
      errors match {
        case Nil => History(score + newHead.score, List(newHead), successes)
        case head :: tail =>
          head.append(newHead) match {
            case None => History(score + newHead.score, newHead :: errors, successes)
            case Some(merged) => History(score - head.score + merged.score, merged :: tail, successes)
          }
      }
    }
  }

  trait ParseError {
    def penalty: Double
    def score: Double = -penalty * 1
    def append(other: ParseError): Option[ParseError] = None
    def message: String
    def from: Input
    def to: Input
    def range: SourceRange

    override def toString = message
  }

  class EditorLazy[Result](_inner: => Self[Result]) extends Lazy[Result](_inner) with EditorParserBase[Result] {
  }

  override def lazyParser[Result](inner: => Self[Result]) = new EditorLazy(inner)

  case class Fail[Result](value: Option[Result], message: String) extends EditorParserBase[Result] with LeafParser[Result] {

    override def getParser(recursive: GetParse): Parse[Result] = {
      (input, _) => newFailure(value, input, new History(GenericError(input, message, HistoryConstants.failPenalty)))
    }

    override def getMustConsume(cache: ConsumeCache) = false
  }
}
