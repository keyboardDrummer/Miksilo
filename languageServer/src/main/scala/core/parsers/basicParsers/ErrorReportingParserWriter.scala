package core.parsers.basicParsers

import core.parsers.core.{NotCorrectingParserWriter, UnambiguousParserWriter}
import util.cache.{Cache, InfiniteCache}

trait ErrorReportingParserWriter extends UnambiguousParserWriter with NotCorrectingParserWriter {
  type ParseResult[+R] = ReportingParseResult[R]
  override type Self[+R] = Parser[R]
  override type ExtraState = Unit

  override def newFailure[R](input: Input, message: String) = Failure(input, message)

  override def fail[Result](message: String) = FailureParser(message)

  override def leftRight[Left, Right, NewResult](left: Parser[Left], right: => Parser[Right], combine: (Left, Right) => NewResult) =
    new LeftRight(left, right, combine)

  override def choice[Result](first: Parser[Result], other: => Parser[Result], leftIsAlwaysBigger: Boolean) =
    new BiggestOfTwo(first, other)

  override def flatMap[Result, NewResult](left: Parser[Result], getRight: Result => Parser[NewResult]): Parser[NewResult] =
    new FlatMap(left, getRight)

  override def map[Result, NewResult](original: Parser[Result], f: Result => NewResult) = new MapParser(original, f)

  case class FailureParser(message: String) extends Parser[Nothing] {
    override def parseInternal(input: Input, state: ParseStateLike) = Failure(input, message)
  }

  class BiggestOfTwo[Result](first: Parser[Result], second: => Parser[Result]) extends Parser[Result] {
    override def parseInternal(input: Input, state: ParseStateLike) = {
      (state.parse(first, input), state.parse(second, input)) match {
        case (firstResult: ParseSuccess[Result], secondResult: ParseSuccess[Result]) =>
          if (firstResult.remainder.offset >= secondResult.remainder.offset) firstResult else secondResult
        case (_:Failure, secondResult) => secondResult
        case (firstResult, _) => firstResult
      }
    }
  }

  class LeftRight[Left, Right, Result](left: Parser[Left], right: Parser[Right], combine: (Left, Right) => Result) extends Parser[Result] {
    override def parseInternal(input: Input, state: ParseStateLike) = {
      state.parse(left, input) match {
        case leftSuccess: ParseSuccess[Left] => state.parse(right, leftSuccess.remainder) match {
          case rightSuccess: ParseSuccess[Right] => rightSuccess.map(r => combine(leftSuccess.result, r))
          case f: Failure => f
        }
        case f: Failure => f
      }
    }
  }

  class FlatMap[Result, NewResult](left: Parser[Result], getRight: Result => Parser[NewResult]) extends Parser[NewResult] {
    override def parseInternal(input: Input, state: ParseStateLike) = {
      state.parse(left, input) match {
        case leftSuccess: ParseSuccess[Result] => state.parse(getRight(leftSuccess.result), leftSuccess.remainder)
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

      parse(input) match {
        case success: ParseSuccess[Result] if !success.remainder.atEnd =>
          Failure(success.remainder, "Did not parse entire input")
        case f => f
      }
    }

    def parse(input: Input): ParseResult[Result] = {

      val state = new PackratParseState(())
      state.parse(parser, input)
    }
  }
}
