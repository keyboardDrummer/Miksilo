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

  type ParseNode = (Input, Parser[Any])
  class ParseState {

    val recursionIntermediates = mutable.HashMap[ParseNode, ParseResult[Any]]()
    val callStack = mutable.HashSet[ParseNode]()
    val recursiveNodes = mutable.HashSet[ParseNode]()

    def putIntermediate(key: ParseNode, value: ParseResult[Any]): Unit = {
      recursionIntermediates.put(key, value)
    }

    def removeIntermediate(node: ParseNode): Unit = {
      recursionIntermediates.remove(node)
    }

    def getRecursiveResult[Result](node: ParseNode): Option[ParseResult[Result]] = {
      if (callStack.contains(node)) {
        recursiveNodes.add(node)
        return Some(recursionIntermediates.getOrElse(node, ParseFailure(None, node._1, "Entered recursive node without a seed value")).
          asInstanceOf[ParseResult[Result]])
      }
      None
    }

    def withNode[T](node: ParseNode, action: () => T): T = {
      callStack.add(node)
      val result = action()
      callStack.remove(node)
      result
    }
  }

  trait Parser[+Result] {
    def parseWhole(inputs: Input): ParseResult[Result] = {
      val cache = new ParseState()
      parse(inputs, cache) match {
        case success: ParseSuccess[Result] =>
          if (success.remainder.finished) success
          else {
            val failedSuccess = ParseFailure(Some(success.result), success.remainder, "Did not parse entire input")
            failedSuccess.getBiggest(success.biggestFailure)
          }
        case f => f
      }
    }

    final def parse(input: Input, state: ParseState): ParseResult[Result] = {
      val node = (input, this)
      state.getRecursiveResult(node) match {
        case None =>
          state.withNode(node, () => {
            var result = parseInner(input, state)
            if (state.recursiveNodes.contains(node) && result.isInstanceOf[ParseSuccess[Result]]) {
              state.putIntermediate(node, result)
              var previousResult = result.asInstanceOf[ParseSuccess[Result]]
              var continue = true
              while (continue) {
                parseInner(input, state) match {
                  case success: ParseSuccess[Result] if success.remainder.offset > previousResult.remainder.offset =>
                    state.putIntermediate(node, success)
                    previousResult = success
                  case _ => continue = false
                }
              }
              result = previousResult
              state.removeIntermediate(node)
            }
            result
          })

        case Some(result) =>
          result
      }
    }

    def parseInner(inputs: Input, cache: ParseState): ParseResult[Result]

    def default: Option[Result]

    def ~[Right](right: => Parser[Right]) = new Sequence(this, right, (a: Result,b: Right) => (a,b))
    def ~<[Right](right: Parser[Right]) = new IgnoreRight(this, right)
    def ~>[Right](right: Parser[Right]) = new IgnoreLeft(this, right)
    def |[Other >: Result](other: => Parser[Other]) = new OrElse[Result, Other, Other](this, other)
    def ^^[NewResult](f: Result => NewResult) = new Map(this, f)
    def manySeparated(separator: Parser[Any]): Parser[List[Result]] =
      new Sequence(this, Many(separator ~> this), (h: Result, t: List[Result]) => h :: t) |
      Return(List.empty)
  }

  case class Return[Result](value: Result) extends Parser[Result] {
    override def parseInner(inputs: Input, cache: ParseState): ParseResult[Result] = ParseSuccess(value, inputs, NoFailure)

    override def default: Option[Result] = Some(value)
  }

  class Lazy[+Result](_inner: => Parser[Result]) extends Parser[Result] {
    lazy val inner = _inner

    override def parseInner(inputs: Input, cache: ParseState): ParseResult[Result] = inner.parseInner(inputs, cache)

    override def default: Option[Result] = inner.default
  }

  class Sequence[+Left, +Right, +Result](left: Parser[Left], _right: => Parser[Right],
                                      combine: (Left, Right) => Result) extends Parser[Result] {
    lazy val right: Parser[Right] = _right

    override def parseInner(input: Input, cache: ParseState): ParseResult[Result] = {
      val leftResult = left.parse(input, cache)
      leftResult match {
        case leftSuccess: ParseSuccess[Left] =>
          val rightResult = right.parse(leftSuccess.remainder, cache)
          rightResult match {
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
    override def parseInner(input: Input, cache: ParseState): ParseResult[Result] = {
      original.parse(input, cache) match {
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
    override def parseInner(inputs: Input, cache: ParseState): ParseResult[List[Result]] = {
      val result = single.parse(inputs, cache)
      result match {
        case success: ParseSuccess[Result] =>
          parse(success.remainder, cache).map(r => success.result :: r)
        case failure: ParseFailure[Result] =>
          val partialResult = failure.partialResult.fold(List.empty[Result])(r => List(r))
          val newFailure = ParseFailure[List[Result]](Some(partialResult), failure.remainder, failure.message)
          ParseSuccess[List[Result]](List.empty, inputs, newFailure) // Voor het doorparsen kan ik kijken of de failure iets geparsed heeft, en zo ja verder parsen op de remainder.
      }
    }

    override def default: Option[List[Result]] = Some(List.empty[Result])
  }

  case class SomeParser[Result](single: Parser[Result]) extends
    Sequence[Result, List[Result], List[Result]](single, Many(single), (f, rest) => f :: rest)

  class OrElse[+First <: Result, +Second <: Result, +Result](first: Parser[First], _second: => Parser[Second])
    extends Parser[Result] {
    lazy val second = _second

    override def parseInner(input: Input, cache: ParseState): ParseResult[Result] = {
      val firstResult = first.parse(input, cache)
      firstResult match {
        case _: ParseSuccess[Result] => firstResult
        case firstFailure: ParseFailure[Result] =>
          val secondResult = second.parse(input, cache)
          secondResult match {
            case secondSuccess: ParseSuccess[Result] =>
              val biggestFailure = firstFailure.getBiggest(secondSuccess.biggestFailure)
              ParseSuccess(secondSuccess.result, secondSuccess.remainder, biggestFailure)
            case secondFailure: ParseFailure[Result] =>
              firstFailure.getBiggest(secondFailure)
          }
        }
      }

    override def default: Option[Result] = first.default.orElse(second.default)
  }

  class Map[+Result, NewResult](original: Parser[Result], f: Result => NewResult) extends Parser[NewResult] {
    override def parseInner(input: Input, cache: ParseState): ParseResult[NewResult] = {
      original.parse(input, cache).map(f)
    }

    override def default: Option[NewResult] = original.default.map(f)
  }
}

