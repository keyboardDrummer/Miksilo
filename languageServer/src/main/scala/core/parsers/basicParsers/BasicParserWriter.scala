package core.parsers.basicParsers

import core.parsers.core.{ParseResultLike, ParserWriters}

trait BasicParserWriter extends ParserWriters {
  type ProcessResult[+R] = ParseResult[R]
  override type Self[+R] = Parser[R]
  override type PState = ParseState

  override def succeed[Result](result: Result) = new SuccessParser(result)

  override def fail[R](input: Input, message: String) = Failure

  override def leftRight[Left, Right, NewResult](left: Parser[Left], right: => Parser[Right], combine: (Left, Right) => NewResult) =
    new LeftRight(left, right, combine)

  override def choice[Result](first: Parser[Result], other: => Parser[Result], leftIsAlwaysBigger: Boolean) =
    new BiggestOfTwo(first, other)

  override def flatMap[Result, NewResult](left: Parser[Result], getRight: Result => Parser[NewResult]): Parser[NewResult] =
    new FlatMap(left, getRight)

  class SuccessParser[+Result](result: Result) extends Parser[Result] {
    override def parseNaively(input: Input, state: ParseState) = ParseSuccess(result, input)
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

  trait ParseResult[+R] extends ParseResultLike[Input, R] {

  }

  case class ParseSuccess[+R](result: R, remainder: Input) extends ParseResult[R] {
    override def getSuccessRemainder = Some(remainder)
  }

  object Failure extends ParseResult[Nothing] {
    override def getSuccessRemainder = None
  }
}
