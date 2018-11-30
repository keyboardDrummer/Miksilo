package core.parsers

import util.cache.{Cache, InfiniteCache}

// TODO misschien proberen door te parsen wanneer er een failure plaatsvindt, zodat ik bij 'missende input' gewoon verder ga. Ik zou ook nog recovery parsers kunnen toevoegen zoals in de paper, maar dat lijkt overkil.

trait EditorParsers extends Parsers {

  type Self[+R] = EditorParser[R]
  type ProcessResult[+R] = ParseResult[Input, R]
  type PF[+R] = ParseFailure[Input, R]
  type PR[+R] = ParseResult[Input, R]
  type PS[+R] = ParseSuccess[Input, R]

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
    final def getDefault(state: ParseState): Option[Result] = getDefault(state.defaultCache)
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
      val state = new ParseState(cache)
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

      def addAlternative[Other >: Result](getAlternative: (EditorParser[Other], EditorParser[Other]) => EditorParser[Other]): EditorParser[Other] = {
        lazy val result: EditorParser[Other] = new EditorLazy(parser | getAlternative(parser, result))
        result
      }

//    override def many[Sum](zero: Sum, reduce: (Result, Sum) => Sum): Processor[Sum] = {
//      lazy val recursive: Processor[Sum] = leftRight(processor, result, reduce).withDefault[Sum](zero)
//      lazy val result: Processor[Sum] = choice(recursive, succeed(zero), leftIsAlwaysBigger = true)
//      result
//    }

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
    override def parseNaively(input: Input, state: ParseState): PR[Result] = {
      original.parseCached(input, state) match {
        case failure: PF[Result] if failure.partialResult.isEmpty =>
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

    override def parseNaively(input: Input, state: ParseState): PR[Result] = {
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

    override def parseNaively(input: Input, cache: ParseState): PR[Result] = {
      val firstResult = first.parseCached(input, cache)
      firstResult match {
        case _: PS[Result] => firstResult
        case firstFailure: PF[Result] =>
          val secondResult = second.parseCached(input, cache)
          secondResult match {
            case secondSuccess: PS[Result] =>
              val biggestFailure = firstFailure.getBiggest(secondSuccess.biggestFailure)
              ParseSuccess(secondSuccess.result, secondSuccess.remainder, biggestFailure)
            case secondFailure: PF[Result] =>
              firstFailure.getBiggest(secondFailure)
          }
      }
    }

    override def getDefault(cache: DefaultCache): Option[Result] = {
      val value: Option[First] = cache(first)
      value.orElse(cache(second))
    }
  }

  class BiggestOfTwo[+First <: Result, +Second <: Result, +Result](first: EditorParser[First], _second: => EditorParser[Second])
    extends EditorParser[Result] {
    lazy val second = _second

    override def parseNaively(input: Input, state: ParseState): PR[Result] = {
      val firstResult = first.parseCached(input, state)
      val secondResult = second.parseCached(input, state)
      (firstResult, secondResult) match {
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
    }

    override def getDefault(cache: DefaultCache): Option[Result] = {
      val value: Option[First] = cache(first)
      value.orElse(cache(second))
    }
  }

  case class Return[+Result](value: Result) extends EditorParser[Result] {
    override def parseNaively(inputs: Input, cache: ParseState): PR[Result] = ParseSuccess(value, inputs, NoFailure)

    override def getDefault(cache: DefaultCache): Option[Result] = Some(value)
  }

  class FlatMap[+Result, +NewResult](left: EditorParser[Result], getRight: Result => EditorParser[NewResult])
    extends EditorParser[NewResult] {

    override def parseNaively(input: Input, state: ParseState): PR[NewResult] = {
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

  case class Fail(message: String) extends EditorParser[Nothing] {
    override def parseNaively(input: Input, cache: ParseState): PR[Nothing] = ParseFailure(None, input, message)

    override def getDefault(cache: DefaultCache): Option[Nothing] = None
  }

  class PositionParser extends EditorParser[Input] {

    override def parseNaively(input: Input, state: ParseState): PR[Input] = {
      ParseSuccess[Input, Input](input, input, NoFailure)
    }

    override def getDefault(cache: DefaultCache): Option[Input] = None
  }

  class WithRemainderParser[Result](original: Parser[Result])
    extends EditorParser[(Result, Input)] {

    override def parseNaively(input: Input, parseState: ParseState): PR[(Result, Input)] = {
      val parseResult = original.parseCached(input, parseState)

      parseResult.map(result => (result, parseResult.remainder))
    }

    override def getDefault(cache: DefaultCache): Option[(Result, Input)] = None
  }

  case class Filter[Other, +Result <: Other](original: EditorParser[Result], predicate: Other => Boolean, getMessage: Other => String)
    extends EditorParser[Result] {
    override def parseNaively(input: Input, state: ParseState): PR[Result] = original.parseNaively(input, state) match {
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