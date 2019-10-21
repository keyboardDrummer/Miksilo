package core.parsers.editorParsers

trait AmbiguityFindingParserWriter extends CorrectingParserWriter {

  override def findBestParseResult[Result](parser: Parser[Result], input: Input, mayStop: StopFunction): SingleParseResult[Result, Input] = {

    val noResultFound = ReadyParseResult(None, input, History.error(FatalError(input, "Grammar is always recursive")))
    var bestResult: ReadyParseResult[Result] = noResultFound

    var resultsSeen = Map.empty[Any, ReadyParseResult[Result]]
    var queue: SortedParseResults[Result] = parser(input, newParseState(input))
    while(queue.nonEmpty) {
      val (parseResult: LazyParseResult[Result], tail) = queue.pop()

      queue = parseResult match {
        case _parseResult: ReadyParseResult[_] =>
          val parseResult = _parseResult.asInstanceOf[ReadyParseResult[Result]]
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
            case tailCons: SRCons[_] =>
              if (mayStop(bestResult.remainder.offset, bestResult.originalScore, tailCons.head.score))
                SREmpty
              else
                tail
            case _ =>
              SREmpty
          }
        case delayedResult: DelayedParseResult[_] =>
          val results = delayedResult.results
          tail.merge(results)
      }
    }
    SingleParseResult(bestResult.resultOption, bestResult.history.errors.toList)
  }

  def getHistoryChoices(history: History[Input]): Seq[(Input, Any)] = {
    history match {
      case withChoices: HistoryWithChoices[Input] => withChoices.choices
      case _ => Seq.empty
    }
  }

  def getHistoryWithoutChoices(history: History[Input]): History[Input] = {
    history match {
      case withChoices: HistoryWithChoices[Input] => withChoices.inner
      case _ => history
    }
  }

  override def choice[Result](first: Self[Result], other: => Self[Result], firstIsLonger: Boolean = false): Self[Result] =
    if (firstIsLonger) new TrackingFirstIsLonger(first, other) else new TrackingChoice(first, other)

  class TrackingFirstIsLonger[+First <: Result, +Second <: Result, Result](val first: Self[First], _second: => Self[Second])
    extends ParserBuilderBase[Result] with ChoiceLike[Result] {

    lazy val second = _second

    override def getParser(recursive: GetParser): Parser[Result] = {
      val parseFirst = recursive(first)
      lazy val parseSecond = recursive(second)

      (input: Input, state: ParseState) => {
        val firstResult = parseFirst(input, state).addHistory(HistoryWithChoices(Seq(input -> first)))
        val secondResult = parseSecond(input, state).addHistory(HistoryWithChoices(Seq(input -> second)))
        firstResult match {
          case cons: SRCons[Result]
            if !cons.head.history.flawed => firstResult
          case _ =>
            firstResult.merge(secondResult)
        }
      }
    }
  }

  class TrackingChoice[+First <: Result, +Second <: Result, Result](val first: Self[First], _second: => Self[Second])
    extends ParserBuilderBase[Result] with ChoiceLike[Result] {

    lazy val second = _second

    override def getParser(recursive: GetParser): Parser[Result] = {
      val parseFirst = recursive(first)
      lazy val parseSecond = recursive(second)

      (input: Input, state: ParseState) => {
        val firstResult = parseFirst(input, state).addHistory(HistoryWithChoices(Seq(input -> first)))
        val secondResult = parseSecond(input, state).addHistory(HistoryWithChoices(Seq(input -> second)))
        val merged = firstResult.merge(secondResult)
        merged
      }
    }
  }
}
