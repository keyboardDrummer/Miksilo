package core.parsers.core

import scala.language.higherKinds

trait ParserWriter {

  type Input <: ParseInput
  type ParseResult[+Result] <: ParseResultLike[Result]
  type Self[+R] <: Parser[R]
  type ExtraState

  case class ParseNode(input: Input, parser: Parser[Any])

  def succeed[Result](result: Result): Self[Result]
  def newSuccess[Result](result: Result, remainder: Input): ParseResult[Result]
  def fail[Result](message: String): Self[Result]
  def lazyParser[Result](inner: => Self[Result]): Self[Result]

  def newFailure[Result](input: Input, message: String): ParseResult[Result]

  def choice[Result](first: Self[Result], other: => Self[Result], leftIsAlwaysBigger: Boolean = false): Self[Result]

  def flatMap[Result, NewResult](left: Self[Result], getRight: Result => Self[NewResult]): Self[NewResult]

  def map[Result, NewResult](original: Self[Result], f: Result => NewResult): Self[NewResult]
    //= flatMap(original, (result: Result) => succeed(f(result)))

  def leftRight[Left, Right, NewResult](left: Self[Left],
                                        right: => Self[Right],
                                        combine: (Left, Right) => NewResult): Self[NewResult]
//  = {
//    flatMap(left, (leftResult: Left) => map(right, (rightResult: Right) => combine(leftResult, rightResult)))
//  }

  implicit class ParserExtensions[+Result](parser: Self[Result]) {

    def addAlternative[Other >: Result](getAlternative: (Self[Other], Self[Other]) => Self[Other]): Self[Other] = {
      lazy val result: Self[Other] = lazyParser(parser | getAlternative(parser, result))
      result
    }

    def |[Other >: Result](other: => Self[Other]) = choice(parser, other)

    def ~[Right](right: => Self[Right]) = leftRight(parser, right, (a: Result, b: Right) => (a, b))

    def ~<[Right](right: Self[Right]) = leftRight(parser, right, Processor.ignoreRight[Result, Right])

    def ~>[Right](right: Self[Right]) = leftRight(parser, right, Processor.ignoreLeft[Result, Right])

    def flatMap[NewResult](getRight: Result => Self[NewResult]): Self[NewResult] =
      ParserWriter.this.flatMap(parser, getRight)

    def map[NewResult](f: Result => NewResult): Self[NewResult] = ParserWriter.this.map(parser, f)

    def option: Self[Option[Result]] = choice(this.map(x => Some(x)), succeed[Option[Result]](None))

    def repN(amount: Int): Self[List[Result]] = {
      if (amount == 0) {
        succeed(List.empty[Result])
      } else {
        leftRight[Result, List[Result], List[Result]](parser, repN(amount - 1), (a,b) => a :: b)
      }
    }

    def many[Sum](zero: Sum, reduce: (Result, Sum) => Sum): Self[Sum] = {
      lazy val result: Self[Sum] = choice(leftRight(parser, result, reduce), succeed(zero), leftIsAlwaysBigger = true)
      result
    }

    def * : Self[List[Result]] = {
      many(List.empty, (h: Result, t: List[Result]) => h :: t)
    }

    def ^^[NewResult](f: Result => NewResult) = map(f)

    def manySeparated(separator: Self[Any]): Self[List[Result]] =
      leftRight(parser, (separator ~> parser).*, (h: Result, t: List[Result]) => h :: t) |
        succeed(List.empty[Result])
  }

  trait Parser[+Result] {
    def parseInternal(input: Input, state: ParseStateLike): ParseResult[Result]
  }

  trait ParseStateLike {
    def parse[Result](parser: Parser[Result], input: Input): ParseResult[Result]
    def extraState: ExtraState
  }

  class EmptyParseState(val extraState: ExtraState) extends ParseStateLike {
    override def parse[Result](parser: Parser[Result], input: Input) = parser.parseInternal(input, this)
  }

  class Lazy[+Result](_inner: => Parser[Result]) extends Parser[Result] {
    lazy val inner: Parser[Result] = _inner

    override def parseInternal(input: Input, state: ParseStateLike): ParseResult[Result] = inner.parseInternal(input, state)
  }

  case class Success[+Result](result: Result, remainder: Input) {
    def map[NewResult](f: Result => NewResult): Success[NewResult] = Success(f(result), remainder)
  }

  trait ParseResultLike[+Result] {
    def map[NewResult](f: Result => NewResult): ParseResult[NewResult] = {
      flatMap(s => newSuccess(f(s.result), s.remainder))
    }

    def flatMap[NewResult](f: Success[Result] => ParseResult[NewResult]): ParseResult[NewResult]
    def successful: Boolean
  }

  class MapParser[Result, NewResult](original: Parser[Result], f: Result => NewResult) extends Parser[NewResult] {
    override def parseInternal(input: Input, state: ParseStateLike) = {
      val result = state.parse(original, input)
      result.map(f)
    }
  }
}

object Processor {
  def ignoreLeft[Left, Right](left: Left, right: Right): Right = right
  def ignoreRight[Left, Right](left: Left, right: Right): Left = left
}

trait ParseInput {
  def offset: Int
  def atEnd: Boolean
}

trait NotCorrectingParserWriter extends ParserWriter {
  type Self[+Result] = Parser[Result]

  def succeed[Result](result: Result): Self[Result] = new SuccessParser(result)
  class SuccessParser[+Result](result: Result) extends Parser[Result] {
    override def parseInternal(input: Input, state: ParseStateLike) = newSuccess(result, input)
  }

}