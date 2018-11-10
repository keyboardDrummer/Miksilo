package core.parsers

import scala.collection.mutable


// TODO misschien proberen door te parsen wanneer er een failure plaatsvindt, zodat ik bij 'missende input' gewoon verder ga. Ik zou ook nog recovery parsers kunnen toevoegen zoals in de paper, maar dat lijkt overkil.
trait Parsers {
  type Input <: InputLike

  trait InputLike {
    def offset: Int
    def finished: Boolean
  }

  trait ParseResult[+Result] {
    def map[NewResult](f: Result => NewResult): ParseResult[NewResult]
  }

  case class ParseSuccess[+Result](result: Result, remainder: Input, biggestFailure: OptionFailure[Result]) extends ParseResult[Result] {
    override def map[NewResult](f: Result => NewResult): ParseSuccess[NewResult] =
      ParseSuccess(f(result), remainder, biggestFailure.map(f))

    def biggestRealFailure: Option[ParseFailure[Result]] = biggestFailure match {
      case failure: ParseFailure[Result] => Some(failure)
      case _ => None
    }

    def addFailure[Other >: Result](other: OptionFailure[Other]): ParseSuccess[Other] =
      if (biggestFailure.offset > other.offset) this else
      ParseSuccess(result, remainder, other)
  }

  trait OptionFailure[+Result] {
    def offset: Int
    def map[NewResult](f: Result => NewResult): OptionFailure[NewResult]
  }

  object NoFailure extends OptionFailure[Nothing] {
    override def offset: Int = -1

    override def map[NewResult](f: Nothing => NewResult): OptionFailure[NewResult] = this
  }

  case class ParseFailure[+Result](partialResult: Option[Result], remainder: Input, message: String) extends ParseResult[Result] with OptionFailure[Result] {
    override def map[NewResult](f: Result => NewResult): ParseFailure[NewResult] = ParseFailure(partialResult.map(r => f(r)), remainder, message)

    override def offset: Int = remainder.offset

    def getBiggest[Other >: Result](other: OptionFailure[Other]): ParseFailure[Other] =
      if (offset > other.offset) this else other.asInstanceOf[ParseFailure[Result]]
  }

  type CacheKey = (Input, Parser[Any])
  class ParseState {
    def remove(cacheKey: CacheKey): Unit = {
      cache.remove(cacheKey)
    }

    val cache = mutable.HashMap[CacheKey, ParseResult[Any]]()
    val parserStack = mutable.HashMap[Parser[Any], Int]()

    def put(key: CacheKey, value: ParseResult[Any]): Unit = {
      cache.put(key, value)
    }

    def pickRecursionLeader(first: Option[Parser[Any]], second: Option[Parser[Any]]): Option[Parser[Any]] = {
      (first, second) match {
        case (Some(f), Some(s)) => Some(if (parserStack(f) > parserStack(s)) f else s)
        case (Some(_), _) => first
        case (_, Some(_)) => second
        case _ => None
      }
    }

    def get[Result](key: CacheKey): Option[ParseMonad[Result]] = {
      cache.get(key).map(result => ParseMonad(
          result.asInstanceOf[ParseResult[Result]],
          Some(key._2).filter(parserStack.contains)))
    }

    def withParser[T](parser: Parser[Any], action: () => T): T = {
      parserStack.put(parser, parserStack.size)
      val result = action()
      parserStack.remove(parser)
      result
    }
  }

  case class ParseMonad[+Result](result: ParseResult[Result], recursionLeader: Option[Parser[Any]] = None) {
    def map[NewResult](f: ParseResult[Result] => ParseResult[NewResult]): ParseMonad[NewResult] = {
      ParseMonad(f(result), recursionLeader)
    }
  }

  trait Parser[+Result] {
    def parseWhole(inputs: Input): ParseResult[Result] = {
      val cache = new ParseState()
      parse(inputs, cache).result match {
        case success: ParseSuccess[Result] =>
          if (success.remainder.finished) success
          else {
            val failedSuccess = ParseFailure(Some(success.result), success.remainder, "Did not parse entire input")
            failedSuccess.getBiggest(success.biggestFailure)
          }
        case f => f
      }
    }

    final def parse(input: Input, cache: ParseState): ParseMonad[Result] = {
      val cacheKey = (input, this)
      cache.get(cacheKey) match {
        case None =>
          cache.withParser(this, () => {
            cache.put(cacheKey, ParseFailure(None, input, "Entered recursion without a seed value"))
            val resultMonad = parseInner(input, cache)
            var result = resultMonad.result
            if (resultMonad.recursionLeader.isEmpty) {
              cache.put(cacheKey, resultMonad.result)
            }
            else if (resultMonad.recursionLeader.contains(this)) {
              cache.put(cacheKey, resultMonad.result)
              var previousResult = result.asInstanceOf[ParseSuccess[Result]]
              var continue = true
              while (continue) {
                val newInner = parseInner(input, cache)
                val newResult = newInner.result.asInstanceOf[ParseSuccess[Result]]
                if (newResult.remainder.offset > previousResult.remainder.offset) {
                  cache.put(cacheKey, resultMonad.result)
                  previousResult = newResult
                } else {
                  continue = false
                }
              }
              result = previousResult
            } else {
              cache.remove(cacheKey) //Remove the recursion seed. // TODO uberhaupt die niet toevoegen.
            }
            ParseMonad(result, resultMonad.recursionLeader)
          })

        case Some(result) =>
          result
      }
    }

    def parseInner(inputs: Input, cache: ParseState): ParseMonad[Result]

    def default: Option[Result]

    def ~[Right](right: => Parser[Right]) = new Sequence(this, right, (a: Result,b: Right) => (a,b))
    def ~<[Right](right: Parser[Right]) = new IgnoreRight(this, right)
    def ~>[Right](right: Parser[Right]) = new IgnoreLeft(this, right)
    def |[Other >: Result](other: Parser[Other]) = new OrElse[Result, Other, Other](this, other)
    def ^^[NewResult](f: Result => NewResult) = new Map(this, f)
    def manySeparated(separator: Parser[Any]): Parser[List[Result]] =
      new Sequence(this, Many(separator ~> this), (h: Result, t: List[Result]) => h :: t) |
      Return(List.empty)
  }

  case class Return[Result](value: Result) extends Parser[Result] {
    override def parseInner(inputs: Input, cache: ParseState): ParseMonad[Result] = ParseMonad(ParseSuccess(value, inputs, NoFailure))

    override def default: Option[Result] = Some(value)
  }

  class Lazy[+Result](_inner: => Parser[Result]) extends Parser[Result] {
    lazy val inner = _inner

    override def parseInner(inputs: Input, cache: ParseState): ParseMonad[Result] = inner.parseInner(inputs, cache)

    override def default: Option[Result] = inner.default
  }

  class Sequence[+Left, +Right, +Result](left: Parser[Left], _right: => Parser[Right],
                                      combine: (Left, Right) => Result) extends Parser[Result] {
    lazy val right: Parser[Right] = _right

    override def parseInner(input: Input, cache: ParseState): ParseMonad[Result] = {
      val leftResult = left.parse(input, cache)
      leftResult.result match {
        case leftSuccess: ParseSuccess[Left] =>
          val rightResult = right.parse(leftSuccess.remainder, cache)
          val parseResult = rightResult.result match {
            case rightSuccess: ParseSuccess[Right] =>
              rightSuccess.map(r => combine(leftSuccess.result, r)).
                addFailure(leftSuccess.biggestFailure.map(l => combine(l, rightSuccess.result)))

            case rightFailure: ParseFailure[Right] =>
              if (leftSuccess.biggestFailure.offset > rightFailure.offset && right.default.nonEmpty) {
                val rightDefault = right.default.get
                leftSuccess.biggestFailure.map(l => combine(l, rightDefault)).asInstanceOf[ParseFailure[Result]]
              }
              else {
                rightFailure.map(right => combine(leftSuccess.result, right))
              }
          }
          ParseMonad(parseResult, cache.pickRecursionLeader(leftResult.recursionLeader, rightResult.recursionLeader))

        case leftFailure: ParseFailure[Left] =>
          val result = for {
            leftPartial <- leftFailure.partialResult
            rightDefault <- right.default
          } yield combine(leftPartial, rightDefault)
          val parseResult = ParseFailure(result, leftFailure.remainder, leftFailure.message)
          ParseMonad(parseResult, leftResult.recursionLeader)
      }
    }

    override def default: Option[Result] = for {
      leftDefault <- left.default
      rightDefault <- right.default
    } yield combine(leftDefault, rightDefault)
  }

  case class WithDefault[Result](original: Parser[Result], _default: Result) extends Parser[Result] {
    override def parseInner(input: Input, cache: ParseState): ParseMonad[Result] = {
      original.parse(input, cache).map {
        case failure: ParseFailure[Result] if failure.partialResult.isEmpty =>
          new ParseFailure[Result](Some(_default), failure.remainder, failure.message)
        case x => x
      }
    }

    override def default: Option[Result] = Some(_default)
  }

  class IgnoreRight[+Result, +Right](left: Parser[Result], right: Parser[Right]) extends //TODO IgnoreRight en IgnoreLeft zouden met een custom implementatie sneller zijn.
    Sequence[Result, Right, Result](left, right, (l,_) => l)
  class IgnoreLeft[+Left, +Result](left: Parser[Left], right: Parser[Result]) extends
    Sequence[Left, Result, Result](left, right, (_,r) => r)

  case class Many[Result](single: Parser[Result]) extends Parser[List[Result]] { //TODO kan ik many ook in termen van de anderen opschrijven?
    override def parseInner(inputs: Input, cache: ParseState): ParseMonad[List[Result]] = {
      val result = single.parse(inputs, cache)
      result.result match {
        case success: ParseSuccess[Result] =>
          val tailResult = parse(success.remainder, cache)
          ParseMonad(tailResult.result.map(r => success.result :: r), cache.pickRecursionLeader(result.recursionLeader, tailResult.recursionLeader))
        case failure: ParseFailure[Result] =>
          val partialResult = failure.partialResult.fold(List.empty[Result])(r => List(r))
          val newFailure = ParseFailure[List[Result]](Some(partialResult), failure.remainder, failure.message)
          val parseResult = ParseSuccess[List[Result]](List.empty, inputs, newFailure) // Voor het doorparsen kan ik kijken of de failure iets geparsed heeft, en zo ja verder parsen op de remainder.
          ParseMonad(parseResult, result.recursionLeader)
      }
    }

    override def default: Option[List[Result]] = Some(List.empty[Result])
  }

  case class SomeParser[Result](single: Parser[Result]) extends
    Sequence[Result, List[Result], List[Result]](single, Many(single), (f, rest) => f :: rest)

  class OrElse[+First <: Result, +Second <: Result, +Result](first: Parser[First], second: Parser[Second])
    extends Parser[Result] {
    override def parseInner(input: Input, cache: ParseState): ParseMonad[Result] = {
      val firstResult = first.parse(input, cache)
      firstResult.result match {
        case _: ParseSuccess[Result] => firstResult
        case firstFailure: ParseFailure[Result] =>
          val secondResult = second.parse(input, cache)
          val parseResult = secondResult.result match {
            case secondSuccess: ParseSuccess[Result] =>
              val biggestFailure = firstFailure.getBiggest(secondSuccess.biggestFailure)
              ParseSuccess(secondSuccess.result, secondSuccess.remainder, biggestFailure)
            case secondFailure: ParseFailure[Result] =>
              firstFailure.getBiggest(secondFailure)
          }
          ParseMonad(parseResult, cache.pickRecursionLeader(firstResult.recursionLeader, secondResult.recursionLeader))
        }
      }

    override def default: Option[Result] = first.default.orElse(second.default)
  }

  class Map[+Result, NewResult](original: Parser[Result], f: Result => NewResult) extends Parser[NewResult] {
    override def parseInner(input: Input, cache: ParseState): ParseMonad[NewResult] = {
      original.parse(input, cache).map(r => r.map(f))
    }

    override def default: Option[NewResult] = original.default.map(f)
  }
}

