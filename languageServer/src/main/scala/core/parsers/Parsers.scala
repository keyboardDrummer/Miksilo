package core.parsers

import scala.collection.mutable

trait InputLike {
  def offset: Int
  def finished: Boolean
}

// TODO misschien proberen door te parsen wanneer er een failure plaatsvindt, zodat ik bij 'missende input' gewoon verder ga. Ik zou ook nog recovery parsers kunnen toevoegen zoals in de paper, maar dat lijkt overkil.
trait Parsers {
  type Input <: InputLike


  trait ParseResult[+Result] {
    def successful: Boolean
    def map[NewResult](f: Result => NewResult): ParseResult[NewResult]
    def get: Result
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

    override def get: Result = result

    override def successful: Boolean = true
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

    override def get: Result = throw new Exception("get was called on a ParseFailure")

    override def successful: Boolean = false

    override def toString: String = message
  }

  case class ParseNode(input: Input, parser: Parser[Any])

  class DefaultCache {
    var values = mutable.Map.empty[Parser[_], Option[_]]

    def apply[Result](parser: Parser[Result]): Option[Result] = {
      values.getOrElseUpdate(parser, {
        values.put(parser, None)
        parser.getDefault(this)
      }).asInstanceOf[Option[Result]]
    }
  }

  class ParseState {

    val defaultCache = new DefaultCache()
    val recursionIntermediates = mutable.HashMap[ParseNode, ParseResult[Any]]()
    val callStack = mutable.HashSet[ParseNode]()
    val nodesWithBackEdges = mutable.HashSet[ParseNode]() //TODO possible this can be only the parsers.

    def putIntermediate(key: ParseNode, value: ParseResult[Any]): Unit = {
      recursionIntermediates.put(key, value)
    }

    def removeIntermediate(node: ParseNode): Unit = {
      recursionIntermediates.remove(node)
    }

    def getPreviousResult[Result](node: ParseNode): Option[ParseResult[Result]] = {
      if (callStack.contains(node)) {
        nodesWithBackEdges.add(node)
        return Some(recursionIntermediates.getOrElse(node, ParseFailure(None, node.input, "Traversed back edge without a previous result")).
          asInstanceOf[ParseResult[Result]])
      }
      None
    }

    def withNodeOnStack[T](node: ParseNode, action: () => T): T = {
      callStack.add(node)
      val result = action()
      callStack.remove(node)
      result
    }
  }

  trait Parser[+Result] {
    def parseWhole(input: Input): ParseResult[Result] = {
      val state = new ParseState()
      parseIteratively(input, state) match {
        case success: ParseSuccess[Result] =>
          if (success.remainder.finished) success
          else {
            val failedSuccess = ParseFailure(Some(success.result), success.remainder, "Did not parse entire input")
            failedSuccess.getBiggest(success.biggestFailure)
          }
        case f => f
      }
    }

    final def parseIteratively(input: Input, state: ParseState): ParseResult[Result] = {
      val node = ParseNode(input, this)
      state.getPreviousResult(node) match {
        case None =>
          state.withNodeOnStack(node, () => {
            var result = parse(input, state)
            result match {
              case success: ParseSuccess[Result] if state.nodesWithBackEdges.contains(node) =>
                result = growResult(node, success, state)
              case _ =>
            }
            result
          })

        case Some(result) =>
          result
      }
    }

    def parse(input: Input, state: ParseState): ParseResult[Result]

    final def getDefault(state: ParseState): Option[Result] = getDefault(state.defaultCache)
    def getDefault(cache: DefaultCache): Option[Result]

    def ~[Right](right: => Parser[Right]) = new Sequence(this, right, (a: Result,b: Right) => (a,b))
    def ~<[Right](right: Parser[Right]) = new IgnoreRight(this, right)
    def ~>[Right](right: Parser[Right]) = new IgnoreLeft(this, right)
    def |[Other >: Result](other: => Parser[Other]) = new OrElse[Result, Other, Other](this, other)
    def map[NewResult](f: Result => NewResult) = new Map(this, f)
    def filter[Other >: Result](predicate: Other => Boolean, getMessage: Other => String) = new Filter(this, predicate, getMessage)

    def * = Many(this)
    def ^^[NewResult](f: Result => NewResult) = new Map(this, f)
    def manySeparated(separator: Parser[Any]): Parser[List[Result]] =
      new Sequence(this, Many(separator ~> this), (h: Result, t: List[Result]) => h :: t) |
      Return(List.empty)
  }

  private def growResult[GrowResult](node: ParseNode, previous: ParseSuccess[GrowResult], state: ParseState): ParseResult[GrowResult] = {
    state.putIntermediate(node, previous)

    node.parser.parse(node.input, state) match {
      case success: ParseSuccess[GrowResult] @unchecked if success.remainder.offset > previous.remainder.offset =>
        growResult(node, success, state)
      case _ =>
        state.removeIntermediate(node)
        previous
    }
  }

  class FlatMap[+Result, +NewResult](left: Parser[Result], f: Result => Parser[NewResult]) extends Parser[NewResult] {

    override def parse(input: Input, state: ParseState): ParseResult[NewResult] = {
      val leftResult = left.parseIteratively(input, state)
      leftResult match {
        case leftSuccess: ParseSuccess[Result] =>
          val right = f(leftSuccess.result)
          val rightResult = right.parseIteratively(leftSuccess.remainder, state)
          rightResult match {
            case rightSuccess: ParseSuccess[NewResult] =>
              rightSuccess.
                addFailure(leftSuccess.biggestFailure match {
                  case NoFailure => NoFailure
                  case ParseFailure(partialResult, remainder, message) => ParseFailure(partialResult.flatMap(r => f(r).getDefault(state)), remainder, message)
                })

            case rightFailure: ParseFailure[NewResult] =>
              if (leftSuccess.biggestFailure.offset > rightFailure.offset && right.getDefault(state).nonEmpty) {
                val rightDefault = right.getDefault(state).get
                leftSuccess.biggestFailure.map(l => rightDefault).asInstanceOf[ParseFailure[NewResult]]
              }
              else {
                rightFailure
              }
          }

        case leftFailure: ParseFailure[Result] =>
          val result = for {
            leftPartial <- leftFailure.partialResult
            rightDefault <- f(leftPartial).getDefault(state)
          } yield rightDefault
          ParseFailure(result, leftFailure.remainder, leftFailure.message)
      }
    }

    override def getDefault(cache: DefaultCache): Option[NewResult] = for {
      leftDefault <- cache(left)
      rightDefault <- cache(f(leftDefault))
    } yield rightDefault
  }

  case class Filter[Other, +Result <: Other](original: Parser[Result], predicate: Other => Boolean, getMessage: Other => String) extends Parser[Result] {
    override def parse(input: Input, state: ParseState): ParseResult[Result] = original.parse(input, state) match {
      case success: ParseSuccess[Result] => if (predicate(success.result)) success else
        ParseFailure(None, success.remainder, getMessage(success.result)).getBiggest(success.biggestFailure)
      case failure: ParseFailure[Result] => ParseFailure(failure.partialResult.filter(predicate), failure.remainder, failure.message)
    }

    override def getDefault(cache: DefaultCache): Option[Result] =
      original.getDefault(cache).filter(predicate)
  }

  case class Return[Result](value: Result) extends Parser[Result] {
    override def parse(inputs: Input, cache: ParseState): ParseResult[Result] = ParseSuccess(value, inputs, NoFailure)

    override def getDefault(cache: DefaultCache): Option[Result] = Some(value)
  }

  case class Fail(message: String) extends Parser[Nothing] {
    override def parse(input: Input, cache: ParseState): ParseResult[Nothing] = ParseFailure(None, input, message)

    override def getDefault(cache: DefaultCache): Option[Nothing] = None
  }

  class Lazy[+Result](_inner: => Parser[Result]) extends Parser[Result] {
    lazy val inner: Parser[Result] = _inner

    override def parse(inputs: Input, cache: ParseState): ParseResult[Result] = inner.parse(inputs, cache)

    override def getDefault(cache: DefaultCache): Option[Result] = cache(inner)
  }

  class Sequence[+Left, +Right, +Result](left: Parser[Left], _right: => Parser[Right],
                                      combine: (Left, Right) => Result) extends Parser[Result] {
    lazy val right: Parser[Right] = _right

    override def parse(input: Input, state: ParseState): ParseResult[Result] = {
      val leftResult = left.parseIteratively(input, state)
      leftResult match {
        case leftSuccess: ParseSuccess[Left] =>
          val rightResult = right.parseIteratively(leftSuccess.remainder, state)
          rightResult match {
            case rightSuccess: ParseSuccess[Right] =>
              rightSuccess.map(r => combine(leftSuccess.result, r)).
                addFailure(leftSuccess.biggestFailure.map(l => combine(l, rightSuccess.result)))

            case rightFailure: ParseFailure[Right] =>
              val maybeRightDefault = right.getDefault(state)
              if (leftSuccess.biggestFailure.offset > rightFailure.offset && maybeRightDefault.nonEmpty) {
                val rightDefault = maybeRightDefault.get
                leftSuccess.biggestFailure.map(l => combine(l, rightDefault)).asInstanceOf[ParseFailure[Result]]
              }
              else {
                rightFailure.map(right => combine(leftSuccess.result, right))
              }
          }

        case leftFailure: ParseFailure[Left] =>
          val result = for {
            leftPartial <- leftFailure.partialResult
            rightDefault <- right.getDefault(state)
          } yield combine(leftPartial, rightDefault)
          ParseFailure(result, leftFailure.remainder, leftFailure.message)
      }
    }

    override def getDefault(cache: DefaultCache): Option[Result] = for {
      leftDefault <- cache(left)
      rightDefault <- cache(right)
    } yield combine(leftDefault, rightDefault)
  }

  case class WithDefault[Result](original: Parser[Result], _default: Result) extends Parser[Result] {
    override def parse(input: Input, cache: ParseState): ParseResult[Result] = {
      original.parseIteratively(input, cache) match {
        case failure: ParseFailure[Result] if failure.partialResult.isEmpty =>
          new ParseFailure[Result](Some(_default), failure.remainder, failure.message)
        case x => x
      }
    }

    override def getDefault(cache: DefaultCache): Option[Result] = Some(_default)
  }

  class IgnoreRight[+Result, +Right](left: Parser[Result], right: Parser[Right]) extends //TODO IgnoreRight en IgnoreLeft zouden met een custom implementatie sneller zijn.
    Sequence[Result, Right, Result](left, right, (l,_) => l)
  class IgnoreLeft[+Left, +Result](left: Parser[Left], right: Parser[Result]) extends
    Sequence[Left, Result, Result](left, right, (_,r) => r)

  case class Many[+Result](single: Parser[Result]) extends Parser[List[Result]] { //TODO kan ik Many ook in termen van de anderen opschrijven?
    override def parse(inputs: Input, cache: ParseState): ParseResult[List[Result]] = {
      val result = single.parseIteratively(inputs, cache)
      result match {
        case success: ParseSuccess[Result] =>
          parseIteratively(success.remainder, cache).map(r => success.result :: r)
        case failure: ParseFailure[Result] =>
          val partialResult = failure.partialResult.fold(List.empty[Result])(r => List(r))
          val newFailure = ParseFailure[List[Result]](Some(partialResult), failure.remainder, failure.message)
          ParseSuccess[List[Result]](List.empty, inputs, newFailure) // Voor het doorparsen kan ik kijken of de failure iets geparsed heeft, en zo ja verder parsen op de remainder.
      }
    }

    override def getDefault(cache: DefaultCache): Option[List[Result]] =
      Some(List.empty[Result])
  }

  case class SomeParser[Result](single: Parser[Result]) extends
    Sequence[Result, List[Result], List[Result]](single, Many(single), (f, rest) => f :: rest)

  class OrElse[+First <: Result, +Second <: Result, +Result](first: Parser[First], _second: => Parser[Second])
    extends Parser[Result] {
    lazy val second = _second

    override def parse(input: Input, cache: ParseState): ParseResult[Result] = {
      val firstResult = first.parseIteratively(input, cache)
      firstResult match {
        case _: ParseSuccess[Result] => firstResult
        case firstFailure: ParseFailure[Result] =>
          val secondResult = second.parseIteratively(input, cache)
          secondResult match {
            case secondSuccess: ParseSuccess[Result] =>
              val biggestFailure = firstFailure.getBiggest(secondSuccess.biggestFailure)
              ParseSuccess(secondSuccess.result, secondSuccess.remainder, biggestFailure)
            case secondFailure: ParseFailure[Result] =>
              firstFailure.getBiggest(secondFailure)
          }
        }
      }


    override def getDefault(cache: DefaultCache): Option[Result] = {
      val value: Option[First] = cache(first)
      value.orElse(cache(second))
    }
  }

  class Map[+Result, NewResult](original: Parser[Result], f: Result => NewResult) extends Parser[NewResult] {
    override def parse(input: Input, cache: ParseState): ParseResult[NewResult] = {
      original.parseIteratively(input, cache).map(f)
    }


    override def getDefault(cache: DefaultCache): Option[NewResult] = cache(original).map(f)
  }
}

