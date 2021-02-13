package miksilo.editorParser.parsers.editorParsers

import miksilo.editorParser.parsers.core._

import scala.collection.mutable

trait CorrectingParserWriter extends OptimizingParserWriter {


  def findBestParseResult[Result](zero: TextPointer, parser: BuiltParser[Result], mayStop: StopFunction,
                                  metrics: Metrics): SingleParseResult[Result] = {

    val start = System.currentTimeMillis()
    val noResultFound = new ReadyParseResult(None, zero, startState, History.error(FatalError(zero, "Grammar is always recursive")))
    var bestResult: ReadyParseResult[State, Result] = noResultFound

    mayStop.reset()

    var cycles = 0
    var queue = parser(zero, startState, newParseState(zero))
    var popResult = queue.pop()
    while(popResult.nonEmpty) {
      cycles += 1
      val (parseResult, tail) = popResult.get

      queue = parseResult match {
        case parseResult: ReadyParseResult[State, Result] =>

          if (bestResult.score < parseResult.score) {
            bestResult = parseResult
          }
          if (false && bestResult.history.spotless) {
            SREmpty.empty[State]
          } else {
            tail
          }
        case delayedResult: DelayedParseResult[State, Result] =>

          if (bestResult.score != noResultFound.score && mayStop(bestResult.remainder.offset, bestResult.score, delayedResult.score)) {
            SREmpty.empty[State]
          } else {
            val results = delayedResult.getResults
            val newTail = tail.merge(results)
            /*
            Caching uitzetten?
            NewTail has a ready with history expected ':<value>' but found ',', expected '}' but found ',', Did not expect ',  "8" : "9" }
            3 errors! while delayedResult only has 1 error. How??
             */
            newTail
          }
      }

      popResult = queue.pop()
    }
    val millisecondsSpent = System.currentTimeMillis() - start
    metrics.measure("Parse trees evaluated", cycles)
    metrics.measure("Parse time", millisecondsSpent)
    SingleParseResult(bestResult.resultOption, bestResult.history.errors.toList)
  }


  def singleResult[Result](parseResult: LazyParseResult[State, Result]): ParseResults[State, Result] =
    ParseResults.singleResult(parseResult)

  def newFailure[Result](position: TextPointer, state: State, error: ParseError): ParseResults[State, Result] =
    singleResult(new ReadyParseResult(None, position, state, History.error(error)))

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

  override def map[Result, NewResult](original: Parser[Result], f: Result => NewResult): Parser[NewResult] = MapParser(original, f)


  class LeftIfRightMoved[+Left, Result](val left: Parser[Left],
                                        _right: => Parser[Result],
                                        combine: (Option[Left], Option[Result]) => Option[Result])
    extends ParserBuilderBase[Result] with SequenceLike[Result] {

    lazy val right: Parser[Result] = _right

    override def getParser(recursive: GetParser): BuiltParser[Result] = {
      val parseLeft = recursive(left)
      lazy val parseRight = recursive(right)

      class LeftIfRightParser extends BuiltParser[Result] {

        def parse(position: TextPointer, state: State, fixPointState: FixPointState, mayFail: Boolean): ParseResult[Result] = {

          def rightFromLeftReady(leftReady: ReadyParseResult[State, Left]): ParseResults[State, Result] = {

            def mapRightResult(rightResult: ReadyParseResult[State, Result]): ReadyParseResult[State, Result] = new ReadyParseResult(
              combine(leftReady.resultOption, rightResult.resultOption),
              rightResult.remainder,
              rightResult.state,
              rightResult.history)

            val rightResult = parseRight(leftReady.remainder, leftReady.state, fixPointState)
            rightResult.flatMap({
              case rightReady: ReadyParseResult[State, Result] =>
                if (rightReady.remainder.offset == leftReady.remainder.offset)
                  SREmpty.empty
                else {
                  singleResult(rightReady.mapWithHistory(mapRightResult, leftReady.history))
                }
              case other => singleResult(other.mapWithHistory(mapRightResult, leftReady.history))
            })
          }

          val withoutLeft = parseRight(position, state, fixPointState)

          // TODO determine if we can delete or comment about the next two lines
          if (position.atEnd())
            return withoutLeft

          val withLeft = parseLeft(position, state, fixPointState).flatMapReady(rightFromLeftReady)
          withoutLeft.merge(withLeft)
        }

        override def apply(position: TextPointer, state: State, fixPointState: FixPointState): ParseResult[Result] = {
          parse(position, state, fixPointState, mayFail = true)
        }

        override def origin: Option[ParserBuilder[Result]] = Some(LeftIfRightMoved.this)
      }
      new LeftIfRightParser
    }

    override def printInner(visited: Set[ParserBuilder[Any]], names: mutable.Map[ParserBuilder[Any], Int]): String =
      s"${left.print(visited, names)} ?~> ${right.print(visited, names)}"
  }

  class Sequence[+Left, +Right, Result](
                                         val left: Parser[Left],
                                        _right: => Parser[Right],
                                        combine: (Option[Left], Option[Right]) => Option[Result])
    extends ParserBuilderBase[Result] with SequenceLike[Result] {

    lazy val right: Parser[Right] = _right

    override def getParser(recursive: GetParser): BuiltParser[Result] = {
      val parseLeft = recursive(left)
      lazy val parseRight = recursive(right)

      new BuiltParser[Result] {
        override def apply(position: TextPointer, state: State, fixPointState: FixPointState) = {
          val leftResults = parseLeft(position, state, fixPointState)

          val delayedLeftResults: ParseResults[State, Left] = leftResults.mapResult({
            case ready: ReadyParseResult[State, Left] =>
              if (ready.history.flawed) {
                new DelayedParseResult[State, Left](ready.remainder, ready.history, () => singleResult(ready))
              }
              else
                ready
            case lazyResult => lazyResult
          })

          def rightFromLeftReady(leftReady: ReadyParseResult[State, Left]): ParseResults[State, Result] = {
            def mapRightResult(rightResult: ReadyParseResult[State, Right]): ReadyParseResult[State, Result] = new ReadyParseResult(
              combine(leftReady.resultOption, rightResult.resultOption),
              rightResult.remainder,
              rightResult.state,
              rightResult.history)

            val rightResult = parseRight(leftReady.remainder, leftReady.state, fixPointState)
            rightResult.mapWithHistory[Result](mapRightResult, leftReady.history)
          }
          delayedLeftResults.flatMapReady(rightFromLeftReady)
        }

        override def origin: Option[ParserBuilder[Result]] = Some(Sequence.this)
      }
    }

    override def printInner(visited: Set[ParserBuilder[Any]], names: mutable.Map[ParserBuilder[Any], Int]): String =
      s"${left.print(visited, names)} ~ ${right.print(visited, names)}"
  }

  class FirstIsLonger[+First <: Result, +Second <: Result, Result](val first: Parser[First], _second: => Parser[Second])
    extends ParserBuilderBase[Result] with ChoiceLike[Result] {

    lazy val second = _second

    override def getParser(recursive: GetParser): BuiltParser[Result] = {
      val parseFirst = recursive(first)
      lazy val parseSecond = recursive(second)

      new BuiltParser[Result] {
        override def apply(position: TextPointer, state: State, fixPointState: FixPointState): ParseResults[State, Result] = {
          val firstResult = parseFirst(position, state, fixPointState)
          firstResult match {
//            case cons: ReadyResults[State, Result]
//              if cons.ready.exists(r => !r._2.history.flawed) => firstResult
            case _ =>
              val secondResult = parseSecond(position, state, fixPointState)
              firstResult.merge(secondResult)
          }
        }

        override def origin: Option[ParserBuilder[Result]] = Some(FirstIsLonger.this)
      }
    }

    override def printInner(visited: Set[ParserBuilder[Any]], names: mutable.Map[ParserBuilder[Any], Int]): String =
      s"(${first.print(visited, names)} | ${second.print(visited, names)})"
  }

  class Choice[+First <: Result, +Second <: Result, Result](val first: ParserBuilder[First], _second: => ParserBuilder[Second])
    extends ParserBuilderBase[Result] with ChoiceLike[Result] {

    lazy val second = _second

    override def getParser(recursive: GetParser): BuiltParser[Result] = {
      val parseFirst = recursive(first)
      lazy val parseSecond = recursive(second)

      new BuiltParser[Result] {
        override def apply(position: TextPointer, state: State, fixPointState: FixPointState): ParseResults[State, Result] = {
          val firstResult = parseFirst(position, state, fixPointState)
          val secondResult = parseSecond(position, state, fixPointState)
          val result = firstResult.merge(secondResult)
          result
        }

        override def origin: Option[ParserBuilder[Result]] = Some(Choice.this)
      }
    }

    override def printInner(visited: Set[ParserBuilder[Any]], names: mutable.Map[ParserBuilder[Any], Int]): String =
      s"(${first.print(visited, names)} | ${second.print(visited, names)})"
  }

  object PositionParser extends ParserBuilderBase[TextPointer] with LeafParser[TextPointer] {

    override def getParser(recursive: GetParser): BuiltParser[TextPointer] = {
      new BuiltParser[TextPointer] {
        override def apply(position: TextPointer, state: State, fixPointState: FixPointState): ParseResult[TextPointer] =
          singleResult(new ReadyParseResult(Some(position), position, state, History.empty))

        override def origin: Option[ParserBuilder[TextPointer]] = Some(PositionParser)
      }
    }

    override def getMustConsume(cache: ConsumeCache) = false
  }

  case class WithRangeParser[Result, NewResult](original: Parser[Result], addRange: (TextPointer, TextPointer, Result) => NewResult)
    extends ParserBuilderBase[NewResult] with ParserWrapper[NewResult] {

    override def getParser(recursive: GetParser): BuiltParser[NewResult] = {
      val parseOriginal = recursive(original)
      new BuiltParser[NewResult] {
        override def apply(position: TextPointer, state: State, fixPointState: FixPointState): ParseResult[NewResult] = {
          parseOriginal(position, state, fixPointState).mapReady(ready => {
            val newValue = ready.resultOption.map(v => addRange(position, ready.remainder, v))
            new ReadyParseResult(newValue, ready.remainder, ready.state, ready.history)
          })
        }

        override def origin: Option[ParserBuilder[NewResult]] = Some(WithRangeParser.this)
      }
    }
  }

  override def succeed[Result](result: Result): Parser[Result] = Succeed(result)

  def newSuccess[Result](result: Result, remainder: TextPointer, state: State, score: Double): ParseResults[State, Result] =
    singleResult(new ReadyParseResult(Some(result), remainder.drop(0), state, History.success(remainder, remainder, result, score)))

  case class Succeed[Result](value: Result) extends ParserBuilderBase[Result] with LeafParser[Result] {

    override def getParser(recursive: GetParser): BuiltParser[Result] = {
      new BuiltParser[Result] {
        override def apply(position: TextPointer, state: State, fixPointState: FixPointState): ParseResult[Result] = newSuccess(value, position, state, 0)

        override def origin: Option[ParserBuilder[Result]] = Some(Succeed.this)
      }
    }

    override def getMustConsume(cache: ConsumeCache) = false

    override def printInner(visited: Set[ParserBuilder[Any]], names: mutable.Map[ParserBuilder[Any], Int]): String = s"Succeed($value)"

  }

  case class MapParser[Result, NewResult](original: Parser[Result], f: Result => NewResult)
    extends ParserBuilderBase[NewResult] with ParserWrapper[NewResult] {

    override def getParser(recursive: GetParser): BuiltParser[NewResult] = {
      val parseOriginal = recursive(original)
      new BuiltParser[NewResult] {
        override def apply(position: TextPointer, state: State, fixPointState: FixPointState): ParseResult[NewResult] =
          parseOriginal(position, state, fixPointState).map(f)

        override def origin: Option[ParserBuilder[NewResult]] = Some(MapParser.this)
      }
    }

    override def printInner(visited: Set[ParserBuilder[Any]], names: mutable.Map[ParserBuilder[Any], Int]): String = s"Map(${original.print(visited, names)})"
  }

  def newFailure[Result](partial: Option[Result], position: TextPointer, state: State, errors: MyHistory) =
    singleResult(new ReadyParseResult(partial, position, state, errors))

  type MyHistory = History

  case class FatalError(location: TextPointer, message: String, penalty: Double = History.failPenalty) extends ParseError {
    override def append(other: ParseError): Option[ParseError] = None

    override def from = location

    override def to = from
  }
}

case class SingleParseResult[+Result](resultOption: Option[Result], errors: List[ParseError]) {
  def successful = errors.isEmpty
  def get: Result = resultOption.get
  def map[NewResult](f: Result => NewResult): SingleParseResult[NewResult] = {
    SingleParseResult(resultOption.map(f), errors)
  }
}
