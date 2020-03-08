package core.parsers.editorParsers

import core.parsers.core.{InputGen, Metrics, ParseText, TextPointer}

trait AmbiguityFindingParserWriter extends CorrectingParserWriter {

  override def findBestParseResult[Result](zero: TextPointer, parser: BuiltParser[Result],
                                           mayStop: StopFunction, metrics: Metrics): SingleParseResult[Result] = {

    val startInput = InputGen(zero, startState)
    val noResultFound = ReadyParseResult(None, startInput, History.error(FatalError(startInput.position, "Grammar is always recursive")))
    var bestResult: ReadyParseResult[State, Result] = noResultFound

    var resultsSeen = Map.empty[Any, ReadyParseResult[State, Result]]
    var queue: ParseResults[State, Result] = parser(startInput, newParseState(startInput))
    while(queue.nonEmpty) {
      val (parseResult: LazyParseResult[State, Result], tail) = queue.pop()

      queue = parseResult match {
        case _parseResult: ReadyParseResult[State, _] =>
          val parseResult = _parseResult.asInstanceOf[ReadyParseResult[State, Result]]
          val parseResultKey = ReadyParseResult(parseResult.resultOption, parseResult.remainder, getHistoryWithoutChoices(parseResult.history))
          if (resultsSeen.contains(parseResultKey)) {
            val previousResult = resultsSeen(parseResultKey)
            val oldChoices = getHistoryChoices(previousResult.history)
            val newChoices = getHistoryChoices(parseResult.history)
            val mixedNoDrop = oldChoices.zip(newChoices)

            // TODO equality check hier moet naar reference kijken.
            val mixed = mixedNoDrop.
              dropWhile(t => System.identityHashCode(t._1) == System.identityHashCode(t._2)).
              takeWhile(t => System.identityHashCode(t._1) != System.identityHashCode(t._2)).reverse
            throw new Exception("Your grammar produces duplicates" + previousResult)
          }
          else
            resultsSeen += parseResultKey -> parseResult

          bestResult = if (bestResult.score >= parseResult.score) bestResult else parseResult
          tail match {
            case tailCons: SRCons[State, _] =>
              if (mayStop(bestResult.remainder.position.offset, bestResult.originalScore, tailCons.head.score))
                SREmpty.empty
              else
                tail
            case _ =>
              SREmpty.empty
          }
        case delayedResult: DelayedParseResult[State, _] =>
          val results = delayedResult.results
          tail.merge(results)
      }
    }
    SingleParseResult(bestResult.resultOption, bestResult.history.errors.toList)
  }

  def getHistoryChoices(history: History): Seq[(TextPointer, Any)] = {
    history match {
      case withChoices: HistoryWithChoices => withChoices.choices
      case _ => Seq.empty
    }
  }

  def getHistoryWithoutChoices(history: History): History = {
    history match {
      case withChoices: HistoryWithChoices => withChoices.inner
      case _ => history
    }
  }

  override def choice[Result](first: Parser[Result], other: => Parser[Result], firstIsLonger: Boolean = false): Parser[Result] =
    if (firstIsLonger) new TrackingFirstIsLonger(first, other) else new TrackingChoice(first, other)

  class TrackingFirstIsLonger[+First <: Result, +Second <: Result, Result](val first: Parser[First], _second: => Parser[Second])
    extends ParserBuilderBase[Result] with ChoiceLike[Result] {

    lazy val second = _second

    override def getParser(recursive: GetParser): BuiltParser[Result] = {
      val parseFirst = recursive(first)
      lazy val parseSecond = recursive(second)

      (input: Input, state: FixPointState) => {
        val firstResult = parseFirst(input, state).addHistory(HistoryWithChoices(Seq(input.position -> first)))
        val secondResult = parseSecond(input, state).addHistory(HistoryWithChoices(Seq(input.position -> second)))
        firstResult match {
          case cons: SRCons[State, Result]
            if !cons.head.history.flawed => firstResult
          case _ =>
            firstResult.merge(secondResult)
        }
      }
    }
  }

  class TrackingChoice[+First <: Result, +Second <: Result, Result](val first: Parser[First], _second: => Parser[Second])
    extends ParserBuilderBase[Result] with ChoiceLike[Result] {

    lazy val second = _second

    override def getParser(recursive: GetParser): BuiltParser[Result] = {
      val parseFirst = recursive(first)
      lazy val parseSecond = recursive(second)

      (input: Input, state: FixPointState) => {
        val firstResult = parseFirst(input, state).addHistory(HistoryWithChoices(Seq(input.position -> first)))
        val secondResult = parseSecond(input, state).addHistory(HistoryWithChoices(Seq(input.position -> second)))
        val merged = firstResult.merge(secondResult)
        merged
      }
    }
  }
}
