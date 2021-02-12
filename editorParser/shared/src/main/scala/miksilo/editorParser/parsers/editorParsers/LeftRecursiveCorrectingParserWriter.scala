package miksilo.editorParser.parsers.editorParsers

import miksilo.editorParser.parsers.caching.ArrayOffsetManager
import miksilo.editorParser.parsers.core._

trait CachingParseResult {
  def latestRemainder: OffsetPointer
}

// TODO consider pushing down the caching part into another ParsingWriter
trait LeftRecursiveCorrectingParserWriter extends CorrectingParserWriter {

  type ParseResult[+Result] <: CachingParseResult

  override def newParseState(position: TextPointer) = FixPointState(position.offset, 0, Set.empty)

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

    override def apply(position: TextPointer, state: State, fixPointState: FixPointState): ParseResult[Result] = {
      val newState = moveState(position, fixPointState)

      val key = (state, parser, newState.callStack)
      position.cache.get(key) match {
        case Some(value) =>
          value.asInstanceOf[ParseResult[Result]]
        case None =>
          getPreviousResult(position, newState) match {
            case Some(intermediate) =>
              intermediate

            case None =>
              if (newState.callStack.contains(parser))
                throw new Exception("recursion should have been detected.")
              val nextState = FixPointState(newState.offset, newState.stackDepth, newState.callStack + parser)
              val initialResult = parser(position, state, nextState)

              val RecursionsList(recursions, resultWithoutRecursion) = recursionsFor(initialResult, parser)
              val foundRecursion = recursions.nonEmpty

              val result = if (foundRecursion)
                growWithTrampoline(recursions, resultWithoutRecursion)
              else
                resultWithoutRecursion

              position.cache.put(key, result)

              result
          }
      }
    }

    def growWithTrampoline(recursions: List[RecursiveParseResult[State, Result, Result]], previous: ParseResult[Result]): ParseResult[Result] = {
      var result: ParseResults[State, Result] = SREmpty.empty[State]
      var current = previous

      while (current.isInstanceOf[ReadyResults[State, Result]]) {
        // The order of the merge determines whether ambiguous grammars are left or right associative by default.
        result = result.merge(current)

        current = current.flatMapReady(growStep(recursions, _))
      }

      val grown = grow(recursions, current)
      val finalResult = grown.merge(result)
      finalResult
    }

    def grow(recursions: List[RecursiveParseResult[State, Result, Result]], previous: ParseResult[Result]): ParseResult[Result] = {
      val next = previous.flatMapReady(prev => {
        val grown = growWithTrampoline(recursions, growStep(recursions, prev))
        // The order of the merge determines whether ambiguous grammars are left or right associative by default.
        val result = grown.merge(singleResult(prev))
        result
      })

      next
    }

    def growStep(recursions: List[RecursiveParseResult[State, Result, Result]], prev: ReadyParseResult[State, Result]): ParseResults[State, Result] = {
      recursions.map((recursive: RecursiveParseResult[State, Result, Result]) => {
        val results = recursive.get(singleResult(prev))
        results.flatMapReady(
          ready => if (ready.remainder.offset > prev.remainder.offset) singleResult(ready) else
            SREmpty.empty) // TODO maybe set this to uniform = true
      }).reduce((a, b) => a.merge(b))
    }

    def getPreviousResult(position: TextPointer, fixPointState: FixPointState): Option[ParseResult[Result]] = {
      if (fixPointState.offset == position.offset && fixPointState.callStack.contains(parser))
        Some(RecursiveResults(Map(parser -> List(RecursiveParseResult[State, Result, Result](x => x))), SREmpty.empty))
      else
        None
    }
  }

  override def wrapParser[Result](parser: BuiltParser[Result],
                                  shouldCache: Boolean,
                                  shouldDetectLeftRecursion: Boolean): BuiltParser[Result] = {
      if (!shouldCache && !shouldDetectLeftRecursion) {
        parser
      } else
      if (shouldDetectLeftRecursion) {
        new DetectFixPointAndCache[Result](parser)
      } else {
        new CheckCache[Result](parser)
      }
  }

  // TODO: replace List with something that has constant concat operation.
  case class RecursiveResults[+Result](recursions: Map[BuiltParser[Any], List[RecursiveParseResult[State, _, Result]]],
                                       tail: ParseResults[State, Result])
    extends ParseResults[State, Result] {

    override def pop() = throw new Exception("Can't pop recursions")

    override def toList = tail.toList

    override def merge[Other >: Result](other: ParseResults[State, Other]): RecursiveResults[Other] = other match {
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

    override def flatMap[NewResult](f: LazyParseResult[State, Result] => ParseResults[State, NewResult]) = {
      RecursiveResults(
        recursions.view.mapValues(s => s.map(r => r.compose(pr => pr.flatMap(f)))).toMap,
        tail.flatMap(f))
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
    if (state.offset == position.offset) state else FixPointState(position.offset, state.stackDepth, Set.empty)

  class CheckCache[Result](parser: BuiltParser[Result]) extends BuiltParser[Result] {
    // TODO I can differentiate between recursive and non-recursive results. Only the former depend on the state.

    def apply(position: TextPointer, state: State, fixPointState: FixPointState): ParseResult[Result] = {
      val newFixPointState =  moveState(position, fixPointState)
      val key = (state, parser, newFixPointState.callStack)

      position.cache.get(key) match {
        case Some(value) =>
          value.asInstanceOf[ParseResult[Result]]
        case _ =>
          val value: ParseResult[Result] = parser(position, state, newFixPointState)

          position.cache.put(key, value)
          value
      }
    }

    override def origin: Option[ParserBuilder[Result]] = None
  }

  def getSingleResultParser[Result](parser: ParserBuilder[Result]): SingleResultParser[Result] = {
    val parserAndCaches = compile(parser).buildParser(parser)
    new SingleResultParser[Result] {

      override def parse(text: String, mayStop: StopFunction, metrics: Metrics) = {
        parse(new ArrayOffsetManager(new ParseText(text), false).getOffsetNode(0), mayStop, metrics)
      }

      override def parse(zero: TextPointer, mayStop: StopFunction, metrics: Metrics) = {
        findBestParseResult(zero, parserAndCaches.parser, mayStop, metrics)
      }
    }
  }
}

