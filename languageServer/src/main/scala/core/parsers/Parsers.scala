package core.parsers

trait InputLike {
  def offset: Int
}

// TODO misschien proberen door te parsen wanneer er een failure plaatsvindt, zodat ik bij 'missende input' gewoon verder ga. Ik zou ook nog recovery parsers kunnen toevoegen zoals in de paper, maar dat lijkt overkil.
trait Parsers {
  type Input <: InputLike

  trait ParseResult[+Result] {
    def map[NewResult](f: Result => NewResult): ParseResult[NewResult]
  }

  case class ParseSuccess[+Result](result: Result, remainder: Input) extends ParseResult[Result] {
    override def map[NewResult](f: Result => NewResult): ParseResult[NewResult] = ParseSuccess(f(result), remainder)
  }

  case class ParseFailure[+Result](partialResult: Option[Result], remainder: Input, message: String) extends ParseResult[Result] {
    override def map[NewResult](f: Result => NewResult): ParseResult[NewResult] = ParseFailure(partialResult.map(r => f(r)), remainder, message)
  }

  trait Parser[+Result] {
    def parse(inputs: Input): ParseResult[Result]
    def default: Option[Result]

    def ~[Right](right: => Parser[Right]) = new Sequence(this, right, (a: Result,b: Right) => (a,b))
    def ~<[Right](right: Parser[Right]) = new IgnoreRight(this, right)
    def ~>[Right](right: Parser[Right]) = new IgnoreLeft(this, right)
    def |[Other >: Result](other: Parser[Other]) = new OrElse[Result, Other, Other](this, other)
  }

  class Sequence[+Left, +Right, +Result](left: Parser[Left], _right: => Parser[Right],
                                      combine: (Left, Right) => Result) extends Parser[Result] {
    lazy val right: Parser[Right] = _right

    override def parse(inputs: Input): ParseResult[Result] = {
      val leftResult = left.parse(inputs)
      leftResult match {
        case leftSuccess: ParseSuccess[Left] =>
          val rightResult = right.parse(leftSuccess.remainder)
          rightResult match {
            case rightSuccess: ParseSuccess[Right] => ParseSuccess(combine(leftSuccess.result, rightSuccess.result), rightSuccess.remainder)
            case rightFailure: ParseFailure[Right] =>
              rightFailure.map(right => combine(leftSuccess.result, right))
        }
        case leftFailure: ParseFailure[Left] =>
          val result = for {
            leftPartial <- leftFailure.partialResult
            rightDefault <- right.default
          } yield combine(leftPartial, rightDefault)
          ParseFailure(result, leftFailure.remainder, leftFailure.message)
      }
    }

    override def default: Option[Result] = for {
      leftDefault <- left.default
      rightDefault <- right.default
    } yield combine(leftDefault, rightDefault)
  }

  case class WithDefault[Result](original: Parser[Result], _default: Result) extends Parser[Result] {
    override def parse(inputs: Input): ParseResult[Result] = original.parse(inputs)

    override def default: Option[Result] = Some(_default)
  }

  class IgnoreRight[+Result, +Right](left: Parser[Result], right: Parser[Right]) extends
    Sequence[Result, Right, Result](left, right, (l,_) => l)
  class IgnoreLeft[+Left, +Result](left: Parser[Left], right: Parser[Result]) extends
    Sequence[Left, Result, Result](left, right, (_,r) => r)

  case class Many[Result](single: Parser[Result]) extends Parser[List[Result]] {
    override def parse(inputs: Input): ParseResult[List[Result]] = {
      val result = single.parse(inputs)
      result match {
        case success: ParseSuccess[Result] => parse(success.remainder).map(r => success.result :: r)
        case failure: ParseFailure[Result] => ParseSuccess(List.empty, inputs) // Voor het doorparsen kan ik kijken of de failure iets geparsed heeft, en zo ja verder parsen op de remainder.
      }
    }

    override def default: Option[List[Result]] = Some(List.empty[Result])
  }

  case class SomeParser[Result](single: Parser[Result]) extends
    Sequence[Result, List[Result], List[Result]](single, Many(single), (f, rest) => f :: rest)

  class OrElse[+First <: Result, +Second <: Result, +Result](first: Parser[First], second: Parser[Second])
    extends Parser[Result] {
    override def parse(inputs: Input): ParseResult[Result] = first.parse(inputs) match {
      case firstSuccess: ParseSuccess[Result] => firstSuccess
      case firstFailure: ParseFailure[Result] => second.parse(inputs) match {
        case secondSuccess: ParseSuccess[Result] => secondSuccess
        case secondFailure: ParseFailure[Result] =>
          if (firstFailure.remainder.offset > secondFailure.remainder.offset) firstFailure
          else secondFailure
      }
    }

    override def default: Option[Result] = first.default.orElse(second.default)
  }
}

