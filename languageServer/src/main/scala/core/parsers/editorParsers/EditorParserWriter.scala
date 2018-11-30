package core.parsers.core

import core.parsers.editorParsers._
import util.cache.{Cache, InfiniteCache}

trait EditorParserWriter extends ParserWriters {

  type Self[+R] = EditorParser[R]
  type ProcessResult[+R] = ParseResult[Input, R]
  type PF[+R] = ParseFailure[Input, R]
  type PR[+R] = ParseResult[Input, R]
  type PS[+R] = ParseSuccess[Input, R]
  type PState = EditorParseState

  override def fail[R](input: Input, message: String): ParseFailure[Input, Nothing] = ParseFailure(None, input, message)

  override def leftRight[Left, Right, NewResult](left: EditorParser[Left],
                                                 right: => EditorParser[Right],
                                                 combine: (Left, Right) => NewResult): EditorParser[NewResult] =
    new Sequence(left, right, combine)

  override def succeed[NR](result: NR): EditorParser[NR] = Return(result)
  def fail(message: String): Fail = Fail(message)

  override def choice[Result](first: EditorParser[Result], other: => EditorParser[Result], leftIsAlwaysBigger: Boolean): EditorParser[Result] =
    if (leftIsAlwaysBigger) new OrElse(first, other) else new BiggestOfTwo(first, other)

  override def flatMap[Result, NewResult](left: EditorParser[Result], f: Result => EditorParser[NewResult]): EditorParser[NewResult] = new FlatMap(left, f)

  trait EditorParser[+Result] extends Parser[Result] with HasGetDefault[Result] {
    final def getDefault(state: PState): Option[Result] = getDefault(state.defaultCache)
  }

  class EditorParseState(resultCache: Cache[ParseNode, ProcessResult[Any]]) extends ParseState(resultCache) {
    val defaultCache = new DefaultCache()
  }

  implicit class EditorParserExtensions[+Result](parser: EditorParser[Result]) extends ParserExtensions(parser) {

    def filter[Other >: Result](predicate: Other => Boolean, getMessage: Other => String) = Filter(parser, predicate, getMessage)

    def withDefault[Other >: Result](_default: Other): EditorParser[Other] =
      WithDefault[Other](parser, cache => Some(_default))

    def parseWholeInput(input: Input,
                        cache: Cache[PN, PR[Any]] = new InfiniteCache()):
      PR[Result] = {

      parse(input, cache)
    }

    def parse(input: Input,
              cache: Cache[PN, PR[Any]] = new InfiniteCache()): PR[Result] = {
      val state = new EditorParseState(cache)
      parser.parseIteratively(input, state) match {
        case success: PS[Result] =>
          if (success.remainder.atEnd) success
          else {
            val failedSuccess = ParseFailure(Some(success.result), success.remainder, "Did not parse entire input")
            failedSuccess.getBiggest(success.biggestFailure)
          }
        case f => f
      }
    }

    override def map[NewResult](f: Result => NewResult) = new MapParser(parser, f)

    def addAlternative[Other >: Result](getAlternative: (EditorParser[Other], EditorParser[Other]) => EditorParser[Other]): EditorParser[Other] = {
      lazy val result: EditorParser[Other] = new EditorLazy(parser | getAlternative(parser, result))
      result
    }

    def withRange[Other >: Result](addRange: (Input, Input, Result) => Other): EditorParser[Other] = {
      val withPosition = new Sequence(
        new PositionParser(),
        new WithRemainderParser(parser),
        (left: Input, resultRight: (Result, Input)) => addRange(left, resultRight._2, resultRight._1))
      WithDefault(withPosition, cache => parser.getDefault(cache))
    }
  }

  case class WithDefault[+Result](original: Parser[Result], _getDefault: DefaultCache => Option[Result])
    extends EditorParser[Result] {
    override def parseNaively(input: Input, state: PState): PR[Result] = {
      original.parseCached(input, state) match {
        case failure: PF[Result] if failure.partialResult.isEmpty || failure.remainder == input =>
          new PF[Result](_getDefault(state.defaultCache), failure.remainder, failure.message)
        case x => x
      }
    }

    override def getDefault(cache: DefaultCache): Option[Result] =
      _getDefault(cache)
  }

  class Sequence[+Left, +Right, +Result](left: EditorParser[Left],
                                         _right: => EditorParser[Right],
                                         combine: (Left, Right) => Result) extends EditorParser[Result] {
    lazy val right: EditorParser[Right] = _right

    override def parseNaively(input: Input, state: PState): PR[Result] = {
      val leftResult = left.parseCached(input, state)
      leftResult match {
        case leftSuccess: PS[Left] =>
          val rightResult = right.parseCached(leftSuccess.remainder, state)
          rightResult match {
            case rightSuccess: PS[Right] =>
              rightSuccess.map(r => combine(leftSuccess.result, r)).
                addFailure(leftSuccess.biggestFailure.map(l => combine(l, rightSuccess.result)))

            case rightFailure: PF[Right] =>
              if (leftSuccess.biggestFailure.offset > rightFailure.offset && rightFailure.partialResult.nonEmpty) {
                val rightDefault = rightFailure.partialResult.get
                leftSuccess.biggestFailure.map(l => combine(l, rightDefault)).asInstanceOf[PF[Result]]
              }
              else {
                rightFailure.map(right => combine(leftSuccess.result, right))
              }
          }

        case leftFailure: PF[Left] =>
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

  class OrElse[+First <: Result, +Second <: Result, +Result](first: EditorParser[First], _second: => EditorParser[Second])
    extends EditorParser[Result] {
    lazy val second = _second

    override def parseNaively(input: Input, state: PState): PR[Result] = {
      val firstResult = first.parseCached(input, state)
      val result = firstResult match {
        case _: PS[Result] => firstResult
        case firstFailure: PF[Result] =>
          val secondResult = second.parseCached(input, state)
          secondResult match {
            case secondSuccess: PS[Result] =>
              val biggestFailure = firstFailure.getBiggest(secondSuccess.biggestFailure)
              ParseSuccess(secondSuccess.result, secondSuccess.remainder, biggestFailure)
            case secondFailure: PF[Result] =>
              firstFailure.getBiggest(secondFailure)
          }
      }
      getDefault(state).fold[PR[Result]](result)(d => result.addDefault[Result](d))
    }

    override def getDefault(cache: DefaultCache): Option[Result] = {
      val value: Option[First] = cache(first)
      value.orElse(cache(second))
    }
  }

  class BiggestOfTwo[+First <: Result, +Second <: Result, +Result](first: EditorParser[First], _second: => EditorParser[Second])
    extends EditorParser[Result] {
    lazy val second = _second

    override def parseNaively(input: Input, state: PState): PR[Result] = {
      val firstResult = first.parseCached(input, state)
      val secondResult = second.parseCached(input, state)
      val result = (firstResult, secondResult) match {
        case (firstSuccess: PS[Result], secondSuccess: PS[Result]) =>
          if (firstSuccess.remainder.offset > secondSuccess.remainder.offset)
            firstSuccess.addFailure(secondSuccess.biggestFailure)
          else
            secondSuccess.addFailure(firstSuccess.biggestFailure)
        case (firstFailure: PF[Result], secondSuccess: PS[Result]) =>
          secondSuccess.addFailure(firstFailure)
        case (firstSuccess: PS[Result], secondFailure: PF[Result]) =>
          firstSuccess.addFailure(secondFailure)
        case (firstFailure: PF[Result], secondFailure: PF[Result]) =>
          firstFailure.getBiggest(secondFailure)
        case _ => throw new Exception("can not occur")
      }
      getDefault(state).fold[PR[Result]](result)(d => result.addDefault[Result](d))
    }

    override def getDefault(cache: DefaultCache): Option[Result] = {
      val value: Option[First] = cache(first)
      value.orElse(cache(second))
    }
  }

  case class Return[+Result](value: Result) extends EditorParser[Result] {
    override def parseNaively(inputs: Input, cache: PState): PR[Result] = ParseSuccess(value, inputs, NoFailure)

    override def getDefault(cache: DefaultCache): Option[Result] = Some(value)
  }

  class FlatMap[+Result, +NewResult](left: EditorParser[Result], getRight: Result => EditorParser[NewResult])
    extends EditorParser[NewResult] {

    override def parseNaively(input: Input, state: PState): PR[NewResult] = {
      val leftResult = left.parseCached(input, state)
      leftResult match {
        case leftSuccess: PS[Result] =>
          val right = getRight(leftSuccess.result)
          val rightResult = right.parseCached(leftSuccess.remainder, state)
          rightResult match {
            case rightSuccess: PS[NewResult] =>
              rightSuccess.
                addFailure(leftSuccess.biggestFailure match {
                  case NoFailure => NoFailure
                  case ParseFailure(partialResult, remainder, message) =>
                    ParseFailure(partialResult.flatMap(leftPartial => getRight(leftPartial).getDefault(state)), remainder, message)
                })

            case rightFailure: PF[NewResult] =>
              if (leftSuccess.biggestFailure.offset > rightFailure.offset) {
                val biggestFailure = leftSuccess.biggestFailure.asInstanceOf[PF[Result]]
                ParseFailure(rightFailure.partialResult, biggestFailure.remainder, biggestFailure.message)
              }
              else {
                rightFailure
              }
          }

        case leftFailure: PF[Result] =>
          val result = for {
            leftPartial <- leftFailure.partialResult
            rightDefault <- getRight(leftPartial).getDefault(state)
          } yield rightDefault
          ParseFailure(result, leftFailure.remainder, leftFailure.message)
      }
    }

    override def getDefault(cache: DefaultCache): Option[NewResult] = for {
      leftDefault <- cache(left)
      rightDefault <- cache(getRight(leftDefault))
    } yield rightDefault
  }

  class MapParser[+Result, NewResult](original: EditorParser[Result], f: Result => NewResult) extends EditorParser[NewResult] {
    override def parseNaively(input: Input, cache: PState): PR[NewResult] = {
      original.parseCached(input, cache).map(f)
    }

    override def getDefault(cache: DefaultCache): Option[NewResult] = cache(original).map(f)
  }

  case class Fail(message: String) extends EditorParser[Nothing] {
    override def parseNaively(input: Input, cache: PState): PR[Nothing] = ParseFailure(None, input, message)

    override def getDefault(cache: DefaultCache): Option[Nothing] = None
  }

  class PositionParser extends EditorParser[Input] {

    override def parseNaively(input: Input, state: PState): PR[Input] = {
      ParseSuccess[Input, Input](input, input, NoFailure)
    }

    override def getDefault(cache: DefaultCache): Option[Input] = None
  }

  class WithRemainderParser[Result](original: Parser[Result])
    extends EditorParser[(Result, Input)] {

    override def parseNaively(input: Input, parseState: PState): PR[(Result, Input)] = {
      val parseResult = original.parseCached(input, parseState)

      parseResult.map(result => (result, parseResult.remainder))
    }

    override def getDefault(cache: DefaultCache): Option[(Result, Input)] = None
  }

  case class Filter[Other, +Result <: Other](original: EditorParser[Result], predicate: Other => Boolean, getMessage: Other => String)
    extends EditorParser[Result] {
    override def parseNaively(input: Input, state: PState): PR[Result] = original.parseNaively(input, state) match {
      case success: PS[Result] =>
        if (predicate(success.result)) success
        else ParseFailure(this.getDefault(state), success.remainder, getMessage(success.result)).getBiggest(success.biggestFailure)
      case failure: PF[Result] =>
        val partialResult = failure.partialResult.filter(predicate).orElse(this.getDefault(state))
        ParseFailure(partialResult, failure.remainder, failure.message)
    }

    override def getDefault(cache: DefaultCache): Option[Result] =
      original.getDefault(cache).filter(predicate)
  }

  class EditorLazy[+Result](_inner: => EditorParser[Result]) extends Lazy[Result](_inner) with EditorParser[Result] {

    override def getDefault(cache: DefaultCache): Option[Result] = cache(inner.asInstanceOf[EditorParser[Result]])
  }
}
