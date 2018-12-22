package core.parsers.ambiguousEditorParsers

import core.parsers.ambigousParsers.AmbiguousParserWriter
import core.parsers.editorParsers.{DefaultCache, EditorParserWriter}
import util.cache.{Cache, InfiniteCache}

trait AmbiguousEditorParserWriter extends AmbiguousParserWriter with EditorParserWriter {

  type ParseResult[+R] = EditorParseResult[R]

  override def combineSuccesses[Result](successes: Seq[EditorParseResult[Result]]) =
    EditorParseResult(successes.flatMap(s => s.successes).toList,
      successes.map(s => s.biggestFailure).fold(NoFailure)((a,b) => a.getBiggest(b)))

  override def failure[Result](partial: Result, input: Input, message: String) =
    EditorParseResult(List.empty, ParseFailure(Some(partial), input, message))

  override def success[Result](result: Result, remainder: Input) =
    EditorParseResult(List(Success(result, remainder)), NoFailure)

  override def failure[R](input: Input, message: String): EditorParseResult[Nothing] = ParseFailure(None, input, message)

  override def leftRight[Left, Right, NewResult](left: EditorParser[Left],
                                                 right: => EditorParser[Right],
                                                 combine: (Left, Right) => NewResult): EditorParser[NewResult] =
    new Sequence(left, right, combine)

  override def succeed[NR](result: NR): EditorParser[NR] = Succeed(result)

  override def choice[Result](first: EditorParser[Result], other: => EditorParser[Result], leftIsAlwaysBigger: Boolean): EditorParser[Result] =
    if (leftIsAlwaysBigger) new Choice(first, other) else new Choice(first, other)

  override def flatMap[Result, NewResult](left: EditorParser[Result], f: Result => EditorParser[NewResult]): EditorParser[NewResult] = ??? //new FlatMap(left, f)

  override def map[Result, NewResult](original: Self[Result], f: Result => NewResult): Self[NewResult] = new MapParser(original, f)

  override def fail[Result](message: String) = Fail(message)

  case class Fail(message: String) extends EditorParser[Nothing] {
    override def getDefault(cache: DefaultCache) = None

    override def parseInternal(input: Input, state: ParseStateLike) = failure(input, message)
  }

  override def lazyParser[Result](inner: => EditorParser[Result]) = new EditorLazy(inner)

  implicit class EditorParserExtensions[+Result](parser: EditorParser[Result]) extends ParserExtensions(parser) {

    def filter[Other >: Result](predicate: Other => Boolean, getMessage: Other => String) = Filter(parser, predicate, getMessage)

    def withDefault[Other >: Result](_default: Other): EditorParser[Other] =
      WithDefault[Other](parser, cache => Some(_default))

    def parseWholeInput(input: Input,
                        cache: Cache[ParseNode, ParseResult[Any]] = new InfiniteCache()): ParseResult[Result] = {

      val result = parse(input, cache)
      if (!result.successful)
        return result

      val resultsAtEnd = result.successes.filter(r => r.remainder.atEnd)
      if (resultsAtEnd.isEmpty) {
        val best = result.successes.maxBy(r => r.remainder.offset)
        val failedSuccess = ParseFailure(Some(best.result), best.remainder, "Did not parse entire input")
        failedSuccess.getBiggest(result.biggestFailure)
      } else {
        EditorParseResult(resultsAtEnd, result.biggestFailure)
      }
    }

    def parse(input: Input,
              cache: Cache[ParseNode, ParseResult[Any]] = new InfiniteCache()): ParseResult[Result] = {

      val state = new PackratParseState(cache, new DefaultCache)
      state.parseIteratively(parser, input)
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
    override def parseInternal(input: Input, state: ParseStateLike): ParseResult[Result] = {
      val result = state.parse(original, input)
      if (result.successes.nonEmpty) {
        return result
      }

      val failure = result.biggestFailure.asInstanceOf[ParseFailure[Result]]
      if (failure.partialResult.isEmpty || failure.remainder == input) {
        return new ParseFailure[Result](_getDefault(state.extraState), failure.remainder, failure.message)
      }
      result
    }

    override def getDefault(cache: DefaultCache): Option[Result] =
      _getDefault(cache)
  }

  class Sequence[+Left, +Right, +Result](left: EditorParser[Left],
                                         _right: => EditorParser[Right],
                                         combine: (Left, Right) => Result) extends EditorParser[Result] {
    lazy val right: EditorParser[Right] = _right

    override def parseInternal(input: Input, state: ParseStateLike): ParseResult[Result] = {
      val leftResult = state.parse(left, input)
      val leftFailure = right.getDefault(state).map(rightDefault => leftResult.biggestFailure.map(l => combine(l, rightDefault))).getOrElse(NoFailure)
      val rightResults = leftResult.successes.map(leftSuccess => {
        val rightResult = state.parse(right, leftSuccess.remainder)
        val endSuccesses = rightResult.successes.map(rightSuccess => {
          Success(combine(leftSuccess.result, rightSuccess.result), rightSuccess.remainder)
        })
        val rightFailure = rightResult.biggestFailure.map(r => combine(leftSuccess.result, r))
        EditorParseResult(endSuccesses, rightFailure)
      })
      combineSuccesses(rightResults).addFailure(leftFailure)
    }

    override def getDefault(cache: DefaultCache): Option[Result] = for {
      leftDefault <- cache(left)
      rightDefault <- cache(right)
    } yield combine(leftDefault, rightDefault)
  }

  class Choice[+First <: Result, +Second <: Result, +Result](first: EditorParser[First], _second: => EditorParser[Second])
    extends EditorParser[Result] {
    lazy val second = _second

    override def parseInternal(input: Input, state: ParseStateLike): ParseResult[Result] = {
      val firstResult = state.parse(first, input)
      val secondResult = state.parse(second, input)
      val result = EditorParseResult[Result](firstResult.successes ++ secondResult.successes,
        firstResult.biggestFailure.getBiggest(secondResult.biggestFailure))
      result
      //getDefault(state).fold[ParseResult[Result]](result)(d => result.addDefault[Result](d))
    }

    override def getDefault(cache: DefaultCache): Option[Result] = {
      val value: Option[First] = cache(first)
      value.orElse(cache(second))
    }
  }

  case class Succeed[+Result](value: Result) extends EditorParser[Result] {
    override def parseInternal(input: Input, cache: ParseStateLike): ParseResult[Result] = EditorParseResult(List(Success[Result](value, input)), NoFailure)

    override def getDefault(cache: DefaultCache): Option[Result] = Some(value)
  }

////  class FlatMap[+Result, +NewResult](left: EditorParser[Result], getRight: Result => EditorParser[NewResult])
////    extends EditorParser[NewResult] {
////
////    override def parseInternal(input: Input, state: ParseStateLike): ParseResult[NewResult] = {
////      val leftResult = state.parse(left, input)
////      leftResult match {
////        case leftSuccess: ParseSuccess[Result] =>
////          val right = getRight(leftSuccess.result)
////          val rightResult = state.parse(right, leftSuccess.remainder)
////          rightResult match {
////            case rightSuccess: ParseSuccess[NewResult] =>
////              rightSuccess.
////                addFailure(leftSuccess.biggestFailure match {
////                  case NoFailure => NoFailure
////                  case ParseFailure(partialResult, remainder, message) =>
////                    ParseFailure(partialResult.flatMap(leftPartial => getRight(leftPartial).getDefault(state)), remainder, message)
////                })
////
////            case rightFailure: ParseFailure[NewResult] =>
////              if (leftSuccess.biggestFailure.offset > rightFailure.offset) {
////                val biggestFailure = leftSuccess.biggestFailure.asInstanceOf[ParseFailure[Result]]
////                ParseFailure(rightFailure.partialResult, biggestFailure.remainder, biggestFailure.message)
////              }
////              else {
////                rightFailure
////              }
////          }
////
////        case leftFailure: ParseFailure[Result] =>
////          val result = for {
////            leftPartial <- leftFailure.partialResult
////            rightDefault <- getRight(leftPartial).getDefault(state)
////          } yield rightDefault
////          ParseFailure(result, leftFailure.remainder, leftFailure.message)
////      }
////    }
//
//    override def getDefault(cache: DefaultCache): Option[NewResult] = for {
//      leftDefault <- cache(left)
//      rightDefault <- cache(getRight(leftDefault))
//    } yield rightDefault
//  }

  class MapParser[+Result, NewResult](original: EditorParser[Result], f: Result => NewResult) extends EditorParser[NewResult] {
    override def parseInternal(input: Input, state: ParseStateLike): ParseResult[NewResult] = {
      state.parse(original, input).map(f)
    }

    override def getDefault(cache: DefaultCache): Option[NewResult] = cache(original).map(f)
  }

  class PositionParser extends EditorParser[Input] {

    override def parseInternal(input: Input, state: ParseStateLike): ParseResult[Input] = {
      EditorParseResult[Input](List(Success(input, input)), NoFailure)
    }

    override def getDefault(cache: DefaultCache): Option[Input] = None
  }

  class WithRemainderParser[Result](original: Parser[Result])
    extends EditorParser[(Result, Input)] {

    override def parseInternal(input: Input, parseState: ParseStateLike): ParseResult[(Result, Input)] = {
      val parseResult = parseState.parse(original, input)

      val newSuccesses = parseResult.successes.map(success => Success((success.result, success.remainder), success.remainder))
      val biggestFailure = parseResult.biggestFailure match {
        case failure: ParseFailure[Result] =>
          ParseFailure(failure.partialResult.map(r => (r, failure.remainder)), failure.remainder, failure.message)
        case NoFailure => NoFailure
      }
      EditorParseResult(newSuccesses, biggestFailure)
    }

    override def getDefault(cache: DefaultCache): Option[(Result, Input)] = None
  }

  case class Filter[Other, +Result <: Other](original: EditorParser[Result],
                                             predicate: Other => Boolean, getMessage: Other => String)
    extends EditorParser[Result] {
    override def parseInternal(input: Input, state: ParseStateLike): ParseResult[Result] = {
      val originalResult = original.parseInternal(input, state)
      var successes = List.empty[Success[Result]]
      var biggestFailure = originalResult.biggestFailure
      for(success <- originalResult.successes) {
        if (predicate(success.result))
          successes ::= success
        else {
          val failure = ParseFailure(this.getDefault(state), success.remainder, getMessage(success.result))
          biggestFailure = biggestFailure.getBiggest(failure)
        }
      }
      EditorParseResult(successes.reverse, biggestFailure)
    }

    override def getDefault(cache: DefaultCache): Option[Result] =
      original.getDefault(cache).filter(predicate)
  }

  class EditorLazy[+Result](_inner: => EditorParser[Result]) extends Lazy[Result](_inner) with EditorParser[Result] {

    override def getDefault(cache: DefaultCache): Option[Result] = cache(inner.asInstanceOf[EditorParser[Result]])
  }

  case class ParseFailure[+Result](partialResult: Option[Result], remainder: Input, message: String)
    extends OptionFailure[Result] {

    if (message == "Traversed back edge without a previous result" && partialResult.nonEmpty) {
      System.out.print("jo")
    }

    override def map[NewResult](f: Result => NewResult): ParseFailure[NewResult] =
      ParseFailure(partialResult.map(r => f(r)), remainder, message)

    override def offset: Int = remainder.offset

    override def toString: String = message

    def addDefault[Other >: Result](value: Other): ParseFailure[Other] = partialResult match {
      case _: Some[Result] => this
      case None => ParseFailure(Some(value), remainder, message)
    }
  }

  case class Success[+Result](result: Result, remainder: Input) {
    def map[NewResult](f: Result => NewResult): Success[NewResult] = Success(f(result), remainder)
  }

  implicit def toResult[Result](biggestFailure: OptionFailure[Result]): EditorParseResult[Result] = EditorParseResult(List.empty, biggestFailure)

  case class EditorParseResult[+Result](successes: List[Success[Result]], biggestFailure: OptionFailure[Result])
    extends AmbiguousParseResult[Result]  {

    def get: Result = successes.head.result

    override def map[NewResult](f: Result => NewResult): EditorParseResult[NewResult] = {
      EditorParseResult[NewResult](successes.map(r => r.map(f)), biggestFailure.map(f))
    }

    def biggestRealFailure: Option[ParseFailure[Result]] = biggestFailure match {
      case failure: ParseFailure[Result] => Some(failure)
      case _ => None
    }

    def addFailure[Other >: Result](other: OptionFailure[Other]): EditorParseResult[Other] =
      if (biggestFailure.offset >= other.offset) this else
        EditorParseResult(successes, other)

    def addDefault[Other >: Result](value: Other) = biggestFailure match {
      case NoFailure => this
      case f: ParseFailure[Result] => EditorParseResult(successes, f.addDefault(value))
    }

    override def getSingleSuccesses =
      successes.map(r => SingleSuccess(EditorParseResult(List(r), biggestFailure), r.remainder))

    override def successful = successes.nonEmpty
  }

  trait OptionFailure[+Result] {
    def offset: Int
    def partialResult: Option[Result]
    def map[NewResult](f: Result => NewResult): OptionFailure[NewResult]

    def getBiggest[Other >: Result](other: OptionFailure[Other]): OptionFailure[Other] = {
      if (offset > other.offset) this else other
    }
  }

  object NoFailure extends OptionFailure[Nothing] {
    override def offset: Int = -1

    override def map[NewResult](f: Nothing => NewResult): OptionFailure[NewResult] = this

    override def partialResult: Option[Nothing] = None
  }

}
