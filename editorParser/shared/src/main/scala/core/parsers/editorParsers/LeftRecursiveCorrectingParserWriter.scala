package core.parsers.editorParsers

import core.parsers.core._

trait CachingParseResult {
  def latestRemainder: TextPointer
}

// TODO consider pushing down the caching part into another ParsingWriter
trait LeftRecursiveCorrectingParserWriter extends CorrectingParserWriter {

  type ParseResult[+Result] <: CachingParseResult

  // TODO CacheKey and Input as type parameters is redundant. Better to have a single State type parameter.

  override def newParseState(input: Input) = FixPointState(input.position.offset, Set.empty)

  def recursionsFor[Result, SeedResult](parseResults: ParseResults[State, Result], parser: BuiltParser[SeedResult]) = {
    parseResults match {
      case recursiveResults: RecursiveResults[Result] =>
        val remainder = recursiveResults.recursions - parser
        RecursionsList(
          recursiveResults.recursions.getOrElse(parser, List.empty).asInstanceOf[List[RecursiveParseResult[State, SeedResult, Result]]],
          if (remainder.isEmpty) recursiveResults.tail else RecursiveResults(remainder, recursiveResults.tail))
      case _ => RecursionsList[State, SeedResult, Result](List.empty, parseResults)
    }
  }

  case class DetectFixPointAndCache[Result](parser: BuiltParser[Result])
    extends CheckCache[Result](parser) {

    override def apply(input: Input, state: FixPointState): ParseResult[Result] = {
      val newState = moveState(input.position, state)

      val key = (input.state, parser, newState.callStack)
      input.position.cache.get(key) match {
        case Some(value) =>
          value.asInstanceOf[ParseResult[Result]]
        case None =>
          getPreviousResult(input, state) match {
            case Some(intermediate) =>
              intermediate

            case None =>
              if (newState.callStack.contains(parser))
                throw new Exception("recursion should have been detected.")
              val nextState = FixPointState(newState.offset, newState.callStack + parser)
              val initialResult = parser(input, nextState)

              val RecursionsList(recursions, resultWithoutRecursion) = recursionsFor(initialResult, parser)
              val foundRecursion = recursions.nonEmpty

              val result = if (foundRecursion)
                grow(recursions, resultWithoutRecursion, initialResult)
              else
                resultWithoutRecursion

              if (result.latestRemainder.offset > input.position.offset) {
                input.position.cache.put(key, result)
              }

              result
          }
      }
    }

    def grow(recursions: List[RecursiveParseResult[State, Result, Result]], previous: ParseResult[Result], initialResults: ParseResult[Result]): ParseResult[Result] = {
      // TODO Consider replacing the previous.merge by moving that inside the lambda.
      previous.merge(previous.flatMapReady(prev => {
        if (prev.history.flawed)
          SREmpty.empty // TODO consider growing this as well
        else {
          val grown: ParseResult[Result] = recursions.map((recursive: RecursiveParseResult[State, Result, Result]) => {
            val results = recursive.get(singleResult(prev))
            results.flatMapReady(
              ready => if (ready.remainder.position.offset > prev.remainder.position.offset) singleResult(ready) else SREmpty.empty,
              uniform = false) // TODO maybe set this to uniform = true
          }).reduce((a,b) => a.merge(b))
          grow(recursions, grown, initialResults)
        }
      }, uniform = false)) // The uniform = false here is because applying recursion is similar to a Sequence
    }

    def getPreviousResult(input: Input, state: FixPointState): Option[ParseResult[Result]] = {
      if (state.offset == input.position.offset && state.callStack.contains(parser))
          Some(RecursiveResults(Map(parser -> List(RecursiveParseResult[State, Result, Result](x => x))), SREmpty.empty))
      else
        None
    }
  }

  override def wrapParser[Result](parser: BuiltParser[Result],
                                  shouldCache: Boolean,
                                  shouldDetectLeftRecursion: Boolean): BuiltParser[Result] = {
      if (!shouldCache && !shouldDetectLeftRecursion) {
        return parser
      }
      if (shouldDetectLeftRecursion) {
        return new DetectFixPointAndCache[Result](parser)
      }
      new CheckCache[Result](parser)
  }

  // TODO: replace List with something that has constant concat operation.
  case class RecursiveResults[+Result](recursions: Map[BuiltParser[Any], List[RecursiveParseResult[State, _, Result]]],
                                       tail: ParseResults[State, Result])
    extends ParseResults[State, Result] {

    override def nonEmpty = false

    override def pop() = throw new Exception("Can't pop recursions")

    override def toList = tail.toList

    override def tailDepth = 0

    override def merge[Other >: Result](other: ParseResults[State, Other], depth: Int,
                                        bests: Map[InputGen[State], Double] = Map.empty): RecursiveResults[Other] = other match {
      case otherRecursions: RecursiveResults[Result] =>
        val merged = this.recursions.foldLeft(otherRecursions.recursions)((acc, entry) => {
          val value = acc.get(entry._1) match {
            case Some(existingValue) => existingValue ++ entry._2
            case None => entry._2
          }
          acc + (entry._1 -> value)
        })
        RecursiveResults(merged, tail.merge(otherRecursions.tail))
      case _ =>
        RecursiveResults(this.recursions, tail.merge(other))
    }

    override def flatMap[NewResult](f: LazyParseResult[State, Result] => ParseResults[State, NewResult], uniform: Boolean) = {
      RecursiveResults(
        recursions.view.mapValues(s => s.map(r => r.compose(pr => pr.flatMap(f, uniform)))).toMap,
        tail.flatMap(f, uniform))
    }

    override def mapWithHistory[NewResult](f: ReadyParseResult[State, Result] => ReadyParseResult[State, NewResult],
                                           oldHistory: History) = {
      if (oldHistory.flawed)
        tail.mapWithHistory(f, oldHistory)
      else
        super.mapWithHistory(f, oldHistory)
    }

    override def latestRemainder = tail.latestRemainder
  }

  def moveState(position: TextPointer, state: FixPointState) =
    if (state.offset == position.offset) state else FixPointState(position.offset, Set.empty)

  class CheckCache[Result](parser: BuiltParser[Result]) extends BuiltParser[Result] {
    // TODO I can differentiate between recursive and non-recursive results. Only the former depend on the state.

    def apply(input: Input, state: FixPointState): ParseResult[Result] = {
      val newState =  moveState(input.position, state)
      val key = (input.state, parser, newState.callStack)

      input.position.cache.get(key) match {
        case Some(value) =>
          value.asInstanceOf[ParseResult[Result]]
        case _ =>
          val value: ParseResult[Result] = parser(input, newState)

          // Do not cache length zero results, since they cannot be corrected moved if something is inserted where they start.
          if (value.latestRemainder.offset > input.position.offset) {
            input.position.cache.put(key, value)
          }
          value
      }
    }
  }

  def getSingleResultParser[Result](parser: ParserBuilder[Result]): SingleResultParser[Result] = {
    val parserAndCaches = compile(parser).buildParser(parser)
    new SingleResultParser[Result] {

      override def parse(text: String, mayStop: StopFunction, metrics: Metrics) = {
        parse(new ArrayOffsetManager(new ParseText(text)).getOffsetNode(0), mayStop, metrics)
      }

      override def parse(zero: TextPointer, mayStop: StopFunction, metrics: Metrics) = {
        findBestParseResult(zero, parserAndCaches.parser, mayStop, metrics)
      }
    }
  }
}

