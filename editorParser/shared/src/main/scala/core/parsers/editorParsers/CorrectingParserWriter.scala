package core.parsers.editorParsers

import core.parsers.core._

trait CorrectingParserWriter extends OptimizingParserWriter {

  type ParseResult[+Result] = ParseResults[Input, Result]

  trait CorrectingInput extends ParseInput {
    def atEnd(): Boolean
  }

  type Input <: CorrectingInput

  def findBestParseResult[Result](startInput: Input, parser: BuiltParser[Result], mayStop: StopFunction,
                                  metrics: Metrics): SingleParseResult[Result, Input] = {

    val start = System.currentTimeMillis()
    val noResultFound = ReadyParseResult(None, startInput, History.error(FatalError(startInput, "Grammar is always recursive")))
    var bestResult: ReadyParseResult[Input, Result] = noResultFound

    mayStop.reset()

    var cycles = 0
    var queue = parser(startInput, newParseState(startInput))
    while(queue.nonEmpty) {
      cycles += 1
      val (parseResult, tail) = queue.pop()

      queue = parseResult match {
        case parseResult: ReadyParseResult[Input, Result] =>

          bestResult = if (bestResult.score >= parseResult.score) bestResult else parseResult
          tail match {
            case tailCons: SRCons[Input, Result] =>
              if (bestResult.history.spotless || mayStop(bestResult.remainder.offset, bestResult.originalScore, tailCons.head.score))
                SREmpty.empty[Input]
              else
                tail
            case _ =>
              SREmpty.empty[Input]
          }
        case delayedResult: DelayedParseResult[Input, Result] =>
          val results = delayedResult.results
          tail.merge(results)
      }
    }
    val millisecondsSpent = System.currentTimeMillis() - start
    metrics.measure("Parse trees evaluated", cycles)
    metrics.measure("Parse time", millisecondsSpent)
    SingleParseResult(bestResult.resultOption, bestResult.history.errors.toList)
  }

  def singleResult[Result](parseResult: LazyParseResult[Input, Result]) =
    new SRCons(parseResult, parseResult.offset, 0, SREmpty.empty)

  def newFailure[Result](error: MyParseError): SRCons[Input, Result] =
    singleResult(ReadyParseResult(None, error.from, History.error(error)))

  def leftRightSimple[Left, Right, Result](left: Parser[Left],
                                           right: => Parser[Right],
                                           combine: (Left, Right) => Result): Parser[Result] = {
    leftRight(left, right, combineSimple(combine))
  }

  def combineSimple[Left, Right, Result](f: (Left, Right) => Result): (Option[Left], Option[Right]) => Option[Result] =
    (ao, bo) => ao.flatMap(a => bo.map(b => f(a, b)))

  def leftRight[Left, Right, Result](left: Parser[Left],
                                     right: => Parser[Right],
                                     combine: (Option[Left], Option[Right]) => Option[Result]): Parser[Result] =
    new Sequence(left, right, combine)

  override def choice[Result](first: Parser[Result], other: => Parser[Result], firstIsLonger: Boolean = false): Parser[Result] =
    if (firstIsLonger) new FirstIsLonger(first, other) else new Choice(first, other)

  override def map[Result, NewResult](original: Parser[Result], f: Result => NewResult): Parser[NewResult] = new MapParser(original, f)


  class LeftIfRightMoved[+Left, Result](val left: Parser[Left],
                                        _right: => Parser[Result],
                                        combine: (Option[Left], Option[Result]) => Option[Result])
    extends ParserBuilderBase[Result] with SequenceLike[Result] {

    lazy val right: Parser[Result] = _right

    override def getParser(recursive: GetParser): BuiltParser[Result] = {
      val parseLeft = recursive(left)
      lazy val parseRight = recursive(right)

      class LeftIfRightParser extends BuiltParser[Result] {

        def parse(input: Input, state: FixPointState, mayFail: Boolean): ParseResult[Result] = {

          def rightFromLeftReady(leftReady: ReadyParseResult[Input, Left]): ParseResults[Input, Result] = {

            def mapRightResult(rightResult: ReadyParseResult[Input, Result]): ReadyParseResult[Input, Result] = ReadyParseResult(
              combine(leftReady.resultOption, rightResult.resultOption),
              rightResult.remainder,
              rightResult.history)

            val rightResult = parseRight(leftReady.remainder, state)
            rightResult.flatMap({
              case rightReady: ReadyParseResult[Input, Result] =>
                if (rightReady.remainder == leftReady.remainder)
                  SREmpty.empty
                else {
                  singleResult(rightReady.mapWithHistory(mapRightResult, leftReady.history))
                }
              case other => singleResult(other.mapWithHistory(mapRightResult, leftReady.history))
            }, uniform = !leftReady.history.canMerge)
          }

          val withoutLeft = parseRight(input, state)

          // TODO determine if we can delete or comment about the next two lines
          if (input.atEnd())
            return withoutLeft

          val withLeft = parseLeft(input, state).flatMapReady(rightFromLeftReady, uniform = false)
          withoutLeft.merge(withLeft)
        }

        override def apply(input: Input, state: FixPointState): ParseResult[Result] = {
          parse(input, state, mayFail = true)
        }
      }
      new LeftIfRightParser
    }
  }

  class Sequence[+Left, +Right, Result](val left: Parser[Left],
                                        _right: => Parser[Right],
                                        combine: (Option[Left], Option[Right]) => Option[Result])
    extends ParserBuilderBase[Result] with SequenceLike[Result] {

    lazy val right: Parser[Right] = _right

    override def getParser(recursive: GetParser): BuiltParser[Result] = {
      val parseLeft = recursive(left)
      lazy val parseRight = recursive(right)

      new BuiltParser[Result] {
        override def apply(input: Input, state: FixPointState) = {
          val leftResults = parseLeft(input, state)

          val delayedLeftResults: ParseResults[Input, Left] = leftResults.mapResult({
            case ready: ReadyParseResult[Input, Left] =>
              if (ready.history.flawed) {
                new DelayedParseResult[Input, Left](ready.remainder, ready.history, () => singleResult(ready))
              }
              else
                ready
            case lazyResult => lazyResult
          }, false) // TODO set to true?

          def rightFromLeftReady(leftReady: ReadyParseResult[Input, Left]): ParseResults[Input, Result] = {
            def mapRightResult(rightResult: ReadyParseResult[Input, Right]): ReadyParseResult[Input, Result] = ReadyParseResult(
              combine(leftReady.resultOption, rightResult.resultOption),
              rightResult.remainder,
              rightResult.history)

            val rightResult = parseRight(leftReady.remainder, state)
            rightResult.mapWithHistory[Result](mapRightResult, leftReady.history)
          }
          delayedLeftResults.flatMapReady(rightFromLeftReady, uniform = false)
        }
      }
    }
  }

  class FirstIsLonger[+First <: Result, +Second <: Result, Result](val first: Parser[First], _second: => Parser[Second])
    extends ParserBuilderBase[Result] with ChoiceLike[Result] {

    lazy val second = _second

    override def getParser(recursive: GetParser): BuiltParser[Result] = {
      val parseFirst = recursive(first)
      lazy val parseSecond = recursive(second)

      new BuiltParser[Result] {
        override def apply(input: Input, state: FixPointState): ParseResults[Input, Result] = {
          val firstResult = parseFirst(input, state)
          val secondResult = parseSecond(input, state)
          firstResult match {
            case cons: SRCons[Input, Result]
              if !cons.head.history.flawed => firstResult
            case _ =>
              firstResult.merge(secondResult)
          }
        }
      }
    }
  }

  class Choice[+First <: Result, +Second <: Result, Result](val first: Parser[First], _second: => Parser[Second])
    extends ParserBuilderBase[Result] with ChoiceLike[Result] {

    lazy val second = _second

    override def getParser(recursive: GetParser): BuiltParser[Result] = {
      val parseFirst = recursive(first)
      lazy val parseSecond = recursive(second)

      new BuiltParser[Result] {
        override def apply(input: Input, state: FixPointState): ParseResults[Input, Result] = {
          val firstResult = parseFirst(input, state)
          val secondResult = parseSecond(input, state)
          firstResult.merge(secondResult)
        }
      }
    }
  }

  object PositionParser extends ParserBuilderBase[Input] with LeafParser[Input] {

    override def getParser(recursive: GetParser): BuiltParser[Input] = {
      (input, _) => {
        singleResult(ReadyParseResult(Some(input), input, History.empty))
      }
    }

    override def getMustConsume(cache: ConsumeCache) = false
  }

  case class WithRangeParser[Result, NewResult](original: Parser[Result], addRange: (TextPointer, TextPointer, Result) => NewResult)
    extends ParserBuilderBase[NewResult] with ParserWrapper[NewResult] {

    override def getParser(recursive: GetParser): BuiltParser[NewResult] = {
      val parseOriginal = recursive(original)
      (input, state) => {
        parseOriginal(input, state).mapReady(ready => {
          val newValue = ready.resultOption.map(v => addRange(input.offsetNode, ready.remainder.offsetNode, v))
          ReadyParseResult(newValue, ready.remainder, ready.history)
        }, uniform = true)
      }
    }
  }

  override def succeed[Result](result: Result): Parser[Result] = Succeed(result)

  def newSuccess[Result](result: Result, remainder: Input, score: Double): SRCons[Input, Result] =
    singleResult(ReadyParseResult(Some(result), remainder, History.success(remainder, remainder, result, score)))

  case class Succeed[Result](value: Result) extends ParserBuilderBase[Result] with LeafParser[Result] {

    override def getParser(recursive: GetParser): BuiltParser[Result] = {
      (input: Input, _) => newSuccess(value, input, 0)
    }

    override def getMustConsume(cache: ConsumeCache) = false
  }

  case class MapParser[Result, NewResult](original: Parser[Result], f: Result => NewResult)
    extends ParserBuilderBase[NewResult] with ParserWrapper[NewResult] {

    override def getParser(recursive: GetParser): BuiltParser[NewResult] = {
      val parseOriginal = recursive(original)
      (input, state) => parseOriginal(input, state).map(f)
    }
  }

  def newFailure[Result](partial: Option[Result], input: Input, errors: MyHistory) =
    singleResult(ReadyParseResult(partial, input, errors))

  type MyParseError = ParseError[Input]
  type MyHistory = History[Input]

  case class FatalError(location: Input, message: String, penalty: Double = History.failPenalty) extends MyParseError {
    override def append(other: MyParseError): Option[MyParseError] = None

    override def from = location

    override def to = from
  }
}

case class SingleParseResult[+Result, Input <: ParseInput](resultOption: Option[Result], errors: List[ParseError[Input]]) {
  def successful = errors.isEmpty
  def get: Result = resultOption.get
}
