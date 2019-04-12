package core.parsers.basicParsers

import core.parsers.core.ParserWriter

trait MachineParserWriter extends ParserWriter {

  type ParseResult[+Result] = SimpleParseResult[Result]
  override type Self[+Result] = Parser[Result]

  def succeed[Result](result: Result): Self[Result] = new SuccessParser(result)

  class SuccessParser[Result](result: Result) extends Parser[Result] {

    override def getParser(recursive: HasRecursive): Parse[Result] = {
      input => newSuccess(result, input)
    }
  }

  override def newSuccess[Result](result: Result, remainder: Input) = SimpleParseResult(Some(Success(result, remainder)))

  override def newFailure[R](input: Input, message: String) = SimpleParseResult(None)

  override def fail[Result](message: String) = FailureParser

  override def leftRight[Left, Right, NewResult](left: Parser[Left], right: => Parser[Right], combine: (Left, Right) => NewResult) =
    Sequence(left, right, combine)

  override def choice[Result](first: Parser[Result], other: => Parser[Result], leftIsAlwaysBigger: Boolean) =
    new BiggestOfTwo(first, other)

  override def map[Result, NewResult](original: Parser[Result], f: Result => NewResult) = MapParser(original, f)

  case class MapParser[Result, NewResult](original: Self[Result], f: Result => NewResult)
    extends Parser[NewResult] {

    override def getParser(recursive: HasRecursive): Parse[NewResult] = {
      val originalParse: Parse[Result] = recursive(original)
      input: Input => {
        val result = originalParse(input)
        result.map[NewResult](f)
      }
    }
  }

  object FailureParser extends Parser[Nothing] {

    override def getParser(recursive: HasRecursive): Parse[Nothing] =
      _ => failureSingleton
  }

  class BiggestOfTwo[Result](val first: Parser[Result], _second: => Parser[Result])
    extends Parser[Result] {

    lazy val second = _second

    override def getParser(recursive: HasRecursive) = {
      val parseFirst = recursive(first)
      val parseSecond = recursive(second)

      input: Input => {
        val firstResult = parseFirst(input)
        val secondResult = parseSecond(input)
        (firstResult.successOption, secondResult.successOption) match {
          case (Some(firstSuccess), Some(secondSuccess)) =>
            if (firstSuccess.remainder.offset >= secondSuccess.remainder.offset) firstResult else secondResult
          case (None, _) => secondResult
          case (_, None) => firstResult
        }
      }
    }
  }

  case class Sequence[Left, Right, Result](left: Parser[Left], right: Parser[Right], combine: (Left, Right) => Result)
    extends Parser[Result] {


    override def getParser(recursive: HasRecursive): Parse[Result] = {
      val leftParser = recursive(left)
      val rightParser = recursive(right)

      input => {
        val leftResult = leftParser(input)
        leftResult.successOption match {
          case None => failureSingleton
          case Some(leftSuccess) => rightParser(leftSuccess.remainder).map(r => combine(leftSuccess.result, r))
        }
      }
    }
  }

  val failureSingleton = new SimpleParseResult[Nothing](None)

  override def abort = failureSingleton

  case class SimpleParseResult[+Result](successOption: Option[Success[Result]]) extends ParseResultLike[Result] { // TODO Don't use nested Option

    override def get = successOption.get.result

    override def map[NewResult](f: Result => NewResult) = SimpleParseResult(successOption.map(s => s.map(f)))

    override def flatMap[NewResult](f: Success[Result] => SimpleParseResult[NewResult]) =
      successOption.fold[SimpleParseResult[NewResult]](failureSingleton)(s => f(s))

    override def resultOption = successOption.map(s => s.result)

    override def successful = successOption.nonEmpty
  }

  implicit class BasicParserExtensions[+Result](parser: Parser[Result]) {

    def parseWholeInput(input: Input): ParseResult[Result] = {

      lazy val r: HasRecursive = new HasRecursive {
        def apply[SomeResult](p: Parser[SomeResult]): Parse[SomeResult] = p.getParser(this)
      }

      val parse = parser.getParser(r)
      val parseResult = parse(input)
      parseResult.successOption match {
        case Some(success) if !success.remainder.atEnd => failureSingleton
        case _ => parseResult
      }
    }
  }
}
