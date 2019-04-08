package core.parsers.basicParsers

import core.parsers.core.{NotCorrectingParserWriter, UnambiguousParserWriter}

trait ErrorReportingParserWriter extends UnambiguousParserWriter with NotCorrectingParserWriter {
  type ParseResult[+R] = ReportingParseResult[R]
  override type Self[+R] = Parser[R]

  override def newFailure[R](input: Input, message: String) = Failure(input, message)

  override def fail[Result](message: String) = FailureParser(message)

  override def leftRight[Left, Right, NewResult](left: Parser[Left], right: => Parser[Right], combine: (Left, Right) => NewResult) =
    new LeftRight(left, right, combine)

  override def choice[Result](first: Parser[Result], other: => Parser[Result], leftIsAlwaysBigger: Boolean) =
    new BiggestOfTwo(first, other)

  override def map[Result, NewResult](original: Parser[Result], f: Result => NewResult) = new MapParser(original, f)

  case class FailureParser(message: String) extends ParserBase[Nothing] with LeafParser[Nothing] {
    override def apply(input: Input) = Failure(input, message)

    override def getMustConsume(cache: ConsumeCache) = false
  }

  class BiggestOfTwo[Result](val first: Parser[Result], _second: => Parser[Result])
    extends ParserBase[Result] with ChoiceLike[Result] {
    lazy val second = _second

    override def apply(input: Input) = {
      (first.parse(input), second.parse(input)) match {
        case (firstResult: ParseSuccess[Result], secondResult: ParseSuccess[Result]) =>
          if (firstResult.remainder.offset >= secondResult.remainder.offset) firstResult else secondResult
        case (_:Failure, secondResult) => secondResult
        case (firstResult, _) => firstResult
      }
    }
  }

  case class LeftRight[Left, Right, Result](left: Parser[Left], right: Parser[Right], combine: (Left, Right) => Result)
    extends ParserBase[Result] with SequenceLike[Result] {

    override def apply(input: Input) = {
      left.parse(input) match {
        case leftSuccess: ParseSuccess[Left] => right.parse(leftSuccess.remainder) match {
          case rightSuccess: ParseSuccess[Right] => rightSuccess.map(r => combine(leftSuccess.result, r))
          case f: Failure => f
        }
        case f: Failure => f
      }
    }
  }

  trait ReportingParseResult[+Result] extends UnambiguousParseResult[Result] { }

  class ParseSuccess[+Result](result: Result, remainder: Input) extends Success[Result](result, remainder) with ReportingParseResult[Result] {
    override def getSuccessRemainder = Some(remainder)

    override def get = result

    override def map[NewResult](f: Result => NewResult) = new ParseSuccess(f(result), remainder)

    override def flatMap[NewResult](f: Success[Result] => ReportingParseResult[NewResult]) = f(this)

    override def resultOption = Some(result)
  }

  case class Failure(remainder: Input, message: String) extends ReportingParseResult[Nothing] {
    override def getSuccessRemainder = None

    override def get = throw new NoSuchElementException("Cannot call get on a Failure")

    override def map[NewResult](f: Nothing => NewResult) = this

    override def flatMap[NewResult](f: Success[Nothing] => ReportingParseResult[NewResult]) = this

    override def resultOption = None
  }

  override def lazyParser[Result](inner: => Parser[Result]) = new Lazy(inner)

  implicit class ReportingParserExtensions[+Result](parser: Parser[Result]) extends ParserExtensions(parser) {

    def parseWholeInput(input: Input): ParseResult[Result] = {

      parseFinal(input) match {
        case success: ParseSuccess[Result] if !success.remainder.atEnd =>
          Failure(success.remainder, "Did not parse entire input")
        case f => f
      }
    }

    def parseFinal(input: Input): ParseResult[Result] = {
      val state = new PackratParseState()
      parser.parse(input)
    }
  }
}
