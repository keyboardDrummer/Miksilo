package core.parsers.core

import scala.language.{existentials, higherKinds}

trait ParserWriter {

  type Input <: ParseInput
  type ParseResult[+Result] <: ParseResultLike[Result]
  type Self[+R] <: Parser[R]
  type ParseState

  def succeed[Result](result: Result): Self[Result]
  def newSuccess[Result](result: Result, remainder: Input): ParseResult[Result]
  def fail[Result](message: String): Self[Result]

  def abort: ParseResult[Nothing]
  def newFailure[Result](input: Input, message: String): ParseResult[Result]

  def choice[Result](first: Self[Result], other: => Self[Result], leftIsAlwaysBigger: Boolean = false): Self[Result]

  def map[Result, NewResult](original: Self[Result], f: Result => NewResult): Self[NewResult]

  def leftRight[Left, Right, NewResult](left: Self[Left],
                                        right: => Self[Right],
                                        combine: (Left, Right) => NewResult): Self[NewResult]

  implicit class ParserExtensions[+Result](parser: Self[Result]) {

    def |[Other >: Result](other: => Self[Other]) = choice(parser, other)

    def ~[Right](right: => Self[Right]) = leftRight(parser, right, (a: Result, b: Right) => (a, b))

    def ~<[Right](right: Self[Right]) = leftRight(parser, right, Processor.ignoreRight[Result, Right])

    def ~>[Right](right: Self[Right]) = leftRight(parser, right, Processor.ignoreLeft[Result, Right])

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

  trait Parse[+Result] {
    def apply(input: Input): ParseResult[Result]
  }

  trait GetParse {
    def apply[Result](parser: Parser[Result]): Parse[Result]
  }

  trait Parser[+Result] {
    def getParser(recursive: GetParse): Parse[Result]
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
    def resultOption: Option[Result]
    def get = resultOption.get
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
