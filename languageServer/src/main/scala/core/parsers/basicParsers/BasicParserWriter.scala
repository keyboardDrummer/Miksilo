package core.parsers.basicParsers

import core.parsers.core.{ParserWriter}
import util.cache.{Cache, InfiniteCache}

trait BasicParserWriter extends ParserWriter {
  type ParseResult[+R] = SimpleParseResult[R]
  override type Self[+R] = Parser[R]
  override type PState = ParseState

  override def succeed[Result](result: Result) = new SuccessParser(result)

  override def failure[R](input: Input, message: String) = Failure

  override def fail[Result](message: String) = FailureParser

  override def leftRight[Left, Right, NewResult](left: Parser[Left], right: => Parser[Right], combine: (Left, Right) => NewResult) =
    new LeftRight(left, right, combine)

  override def choice[Result](first: Parser[Result], other: => Parser[Result], leftIsAlwaysBigger: Boolean) =
    new BiggestOfTwo(first, other)

  override def flatMap[Result, NewResult](left: Parser[Result], getRight: Result => Parser[NewResult]): Parser[NewResult] =
    new FlatMap(left, getRight)

  override def map[Result, NewResult](original: Parser[Result], f: Result => NewResult) = new MapParser(original, f)

  class SuccessParser[+Result](result: Result) extends Parser[Result] {
    override def parseNaively(input: Input, state: ParseState) = ParseSuccess(result, input)
  }

  object FailureParser extends Parser[Nothing] {
    override def parseNaively(input: Input, state: ParseState) = Failure
  }

  class BiggestOfTwo[Result](first: Parser[Result], second: => Parser[Result]) extends Parser[Result] {
    override def parseNaively(input: Input, state: ParseState) = {
      (first.parseCached(input, state), second.parseCached(input, state)) match {
        case (firstResult: ParseSuccess[Result], secondResult: ParseSuccess[Result]) =>
          if (firstResult.remainder.offset >= secondResult.remainder.offset) firstResult else secondResult
        case (Failure, secondResult) => secondResult
        case (firstResult, _) => firstResult
      }
    }
  }

  class LeftRight[Left, Right, Result](left: Parser[Left], right: Parser[Right], combine: (Left, Right) => Result) extends Parser[Result] {
    override def parseNaively(input: Input, state: ParseState) = {
      left.parseCached(input, state) match {
        case Failure => Failure
        case ParseSuccess(leftResult, leftRemainder) => right.parseCached(leftRemainder, state) match {
          case Failure => Failure
          case ParseSuccess(rightResult, rightRemainder) => ParseSuccess(combine(leftResult, rightResult), rightRemainder)
        }
      }
    }
  }

  class FlatMap[Result, NewResult](left: Parser[Result], getRight: Result => Parser[NewResult]) extends Parser[NewResult] {
    override def parseNaively(input: Input, state: ParseState) = {
      left.parseCached(input, state) match {
        case Failure => Failure
        case ParseSuccess(leftResult, leftRemainder) => getRight(leftResult).parseCached(leftRemainder, state)
      }
    }
  }

  trait SimpleParseResult[+R] extends ParseResultLike[R] {}

  case class ParseSuccess[+R](result: R, remainder: Input) extends SimpleParseResult[R] {
    override def getSuccessRemainder = Some(remainder)

    override def get = result

    override def map[NewResult](f: R => NewResult) = ParseSuccess(f(result), remainder)
  }

  object Failure extends SimpleParseResult[Nothing] {
    override def getSuccessRemainder = None

    override def get = throw new NoSuchElementException("Cannot call get on a Failure")

    override def map[NewResult](f: Nothing => NewResult) = this
  }

  override def lazyParser[Result](inner: => Parser[Result]) = new Lazy(inner)

  implicit class BasicParserExtensions[+Result](parser: Parser[Result]) extends ParserExtensions(parser) {

    def parseWholeInput(input: Input,
                        cache: Cache[PN, ParseResult[Any]] = new InfiniteCache()): ParseResult[Result] = {

      parse(input, cache) match {
        case success: ParseSuccess[Result] if !success.remainder.atEnd => Failure
        case f => f
      }
    }

    def parse(input: Input,
              cache: Cache[PN, ParseResult[Any]] = new InfiniteCache()): ParseResult[Result] = {

      val state = new ParseState(cache)
      parser.parseIteratively(input, state)
    }
  }
}
