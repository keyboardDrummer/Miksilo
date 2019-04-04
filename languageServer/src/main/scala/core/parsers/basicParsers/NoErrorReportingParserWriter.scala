package core.parsers.basicParsers

import core.parsers.core.{NotCorrectingParserWriter, UnambiguousParserWriter}

trait NoErrorReportingParserWriter extends UnambiguousParserWriter with NotCorrectingParserWriter {

  type ParseResult[+R] = SimpleParseResult[R]
  override type Self[+R] = Parser[R]

  override def newSuccess[Result](result: Result, remainder: Input) = SimpleParseResult(Some(Success(result, remainder)))

  override def succeed[Result](result: Result) = new SuccessParser(result)

  override def newFailure[R](input: Input, message: String) = SimpleParseResult(None)

  override def fail[Result](message: String) = FailureParser

  override def leftRight[Left, Right, NewResult](left: Parser[Left], right: => Parser[Right], combine: (Left, Right) => NewResult) =
    new LeftRight(left, right, combine)

  override def choice[Result](first: Parser[Result], other: => Parser[Result], leftIsAlwaysBigger: Boolean) =
    new BiggestOfTwo(first, other)

  override def flatMap[Result, NewResult](left: Parser[Result], getRight: Result => Parser[NewResult]): Parser[NewResult] =
    new FlatMap(left, getRight)

  override def map[Result, NewResult](original: Parser[Result], f: Result => NewResult) = new MapParser(original, f)

  object FailureParser extends ParserBase[Nothing] {
    override def parseInternal(input: Input) = failureSingleton

    override def children = List.empty
  }

  class BiggestOfTwo[Result](first: Parser[Result], second: => Parser[Result]) extends ParserBase[Result] {
    override def parseInternal(input: Input) = {
      val firstResult = first.parse(input)
      val secondResult = second.parse(input)
      (firstResult.successOption, secondResult.successOption) match {
        case (Some(firstSuccess), Some(secondSuccess)) =>
          if (firstSuccess.remainder.offset >= secondSuccess.remainder.offset) firstResult else secondResult
        case (None, _) => secondResult
        case (_, None) => firstResult
      }
    }

    override def children = List(first, second)
  }

  class LeftRight[Left, Right, Result](left: Parser[Left], right: Parser[Right], combine: (Left, Right) => Result) extends ParserBase[Result] {
    override def parseInternal(input: Input) = {
      val leftResult = left.parse(input)
      leftResult.successOption match {
        case None => failureSingleton
        case Some(leftSuccess) => right.parse(leftSuccess.remainder).map(r => combine(leftSuccess.result, r))
      }
    }

    override def children = List(left, right)
  }

  class FlatMap[Result, NewResult](left: Parser[Result], getRight: Result => Parser[NewResult]) extends ParserBase[NewResult] {
    override def parseInternal(input: Input) = {
      left.parse(input).successOption match {
        case None => failureSingleton
        case Some(leftSuccess) => getRight(leftSuccess.result).parse(leftSuccess.remainder)
      }
    }

    override def children = ???
  }

  val failureSingleton = new SimpleParseResult[Nothing](None)

  override def abort = failureSingleton

  case class SimpleParseResult[+Result](successOption: Option[Success[Result]]) extends UnambiguousParseResult[Result] { // TODO Don't use nested Option

    override def getSuccessRemainder = successOption.map(s => s.remainder)

    override def get = successOption.get.result

    override def map[NewResult](f: Result => NewResult) = SimpleParseResult(successOption.map(s => s.map(f)))

    override def flatMap[NewResult](f: Success[Result] => SimpleParseResult[NewResult]) =
      successOption.fold[SimpleParseResult[NewResult]](failureSingleton)(s => f(s))

    override def resultOption = successOption.map(s => s.result)
  }

  override def lazyParser[Result](inner: => Parser[Result]) = new Lazy(inner)

  override def newParseState(root: Parser[_]) = new PackratParseState()

  implicit class BasicParserExtensions[+Result](parser: Parser[Result]) extends ParserExtensions(parser) {

    def parseWholeInput(input: Input): ParseResult[Result] = {

      val parseResult = parseFinal(input)
      parseResult.successOption match {
        case Some(success) if !success.remainder.atEnd => failureSingleton
        case _ => parseResult
      }
    }

    def parseFinal(input: Input): ParseResult[Result] = {
      val state = newParseState(parser)
      parser.parse(input)
    }
  }
}
