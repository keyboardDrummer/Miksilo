package core.parsers.ambiguousEditorParsers

import core.parsers.ambigousParsers.AmbiguousParserWriter
import core.parsers.editorParsers.{DefaultCache, EditorParserWriter}

trait AmbiguousEditorParserWriter extends AmbiguousParserWriter with EditorParserWriter {

  type ParseResult[+Result] = EditorParseResult[Result]

  override def combineSuccesses[Result](parseResults: Seq[EditorParseResult[Result]]) =
    EditorParseResult(parseResults.flatMap(s => s.successes).toList,
      parseResults.map(s => s.biggestFailure).fold(NoFailure)((a, b) => a.getBiggest(b)))

  override def newFailure[Result](partial: Option[Result], input: Input, message: String) =
    EditorParseResult(List.empty, ParseFailure(partial, input, message))

  override def newSuccess[Result](result: Result, remainder: Input) =
    EditorParseResult(List(Success(result, remainder)), NoFailure)

  override def newParseState() = new PackratParseState(new DefaultCache)

  override def newFailure[Result](input: Input, message: String): EditorParseResult[Nothing] = ParseFailure(None, input, message)

  override def leftRight[Left, Right, NewResult](left: EditorParser[Left],
                                                 right: => EditorParser[Right],
                                                 combine: (Left, Right) => NewResult): EditorParser[NewResult] =
    new Sequence(left, right, combine)

  override def choice[Result](first: EditorParser[Result], other: => EditorParser[Result], leftIsAlwaysBigger: Boolean): EditorParser[Result] =
    if (leftIsAlwaysBigger) new Choice(first, other) else new Choice(first, other)

  override def flatMap[Result, NewResult](left: EditorParser[Result], f: Result => EditorParser[NewResult]): EditorParser[NewResult] = ???

  override def map[Result, NewResult](original: Self[Result], f: Result => NewResult): Self[NewResult] = new MapParser(original, f)

  override def parseWholeInput[Result](parser: EditorParser[Result],
                                       input: Input): EditorParseResult[Result] = {
    val result = parser.parse(input)
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
    }

    override def getDefault(cache: DefaultCache): Option[Result] = {
      val value: Option[First] = cache(first)
      value.orElse(cache(second))
    }
  }

  class MapParser[+Result, NewResult](original: EditorParser[Result], f: Result => NewResult) extends EditorParser[NewResult] {
    override def parseInternal(input: Input, state: ParseStateLike): ParseResult[NewResult] = {
      state.parse(original, input).map(f)
    }

    override def getDefault(cache: DefaultCache): Option[NewResult] = cache(original).map(f)
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

  implicit def toResult[Result](biggestFailure: OptionFailure[Result]): EditorParseResult[Result] = EditorParseResult(List.empty, biggestFailure)

  override def abort = EditorParseResult(List.empty, NoFailure)

  case class EditorParseResult[+Result](successes: List[Success[Result]], biggestFailure: OptionFailure[Result])
    extends AmbiguousParseResult[Result] with EditorResult[Result] {

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

    override def flatMap[NewResult](f: Success[Result] => ParseResult[NewResult]) = {
      val newSuccesses = successes.map(success => f(success))
      val failure = biggestFailure match {
        case failure: ParseFailure[Result] =>
          val newResult = failure.partialResult.flatMap(r => f(Success(r, failure.remainder)).resultOption)
          ParseFailure(newResult, failure.remainder, failure.message)
        case NoFailure => NoFailure
      }
      combineSuccesses(newSuccesses).addFailure(failure)
    }

    override def resultOption = successes.headOption.map(s => s.result).orElse(biggestFailure.partialResult)

    override def updateRemainder(f: Input => Input) = ???
  }

}
