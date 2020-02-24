package core.parsers.editorParsers

import scala.collection.mutable

trait LeftRecursiveCorrectingParserWriter extends CorrectingParserWriter {

  type ParseState = FixPointState

  override def newParseState(input: Input) = FixPointState(input, Set.empty)

  case class FixPointState(input: Input, parsers: Set[BuiltParser[Any]])

  def recursionsFor[Result, SeedResult](parseResults: ParseResults[Input, Result], parser: BuiltParser[SeedResult]) = {
    parseResults match {
      case recursiveResults: RecursiveResults[Result] =>
        val remainder = recursiveResults.recursions - parser
        RecursionsList(
          recursiveResults.recursions.getOrElse(parser, List.empty).asInstanceOf[List[RecursiveParseResult[Input, SeedResult, Result]]],
          if (remainder.isEmpty) recursiveResults.tail else RecursiveResults(remainder, recursiveResults.tail))
      case _ => RecursionsList[Input, SeedResult, Result](List.empty, parseResults)
    }
  }

  case class DetectFixPointAndCache[Result](parser: BuiltParser[Result]) extends CheckCache[Result](parser) {

    override def apply(input: Input, state: ParseState): ParseResult[Result] = {
      val newState = moveState(input, state)
      val key = (input, newState.parsers)
      cache.get(key) match {
        case Some(value) =>
          value
        case None =>
          getPreviousResult(input, state) match {
            case Some(intermediate) =>
              intermediate

            case None =>

              if (newState.parsers.contains(parser))
                throw new Exception("recursion should have been detected.")
              val nextState = FixPointState(input, newState.parsers + parser)
              val initialResult = parser(input, nextState)

              val RecursionsList(recursions, resultWithoutRecursion) = recursionsFor(initialResult, parser)
              val foundRecursion = recursions.nonEmpty

              val result = if (foundRecursion)
                grow(recursions, resultWithoutRecursion, initialResult)
              else
                resultWithoutRecursion

              cache.put(key, result)
              result
          }
      }
    }

    def grow(recursions: List[RecursiveParseResult[Input, Result, Result]], previous: ParseResult[Result], initialResults: ParseResult[Result]): ParseResult[Result] = {
      // TODO Consider replacing the previous.merge by moving that inside the lambda.
      previous.merge(previous.flatMapReady(prev => {
        if (prev.history.flawed)
          SREmpty.empty // TODO consider growing this as well
        else {
          val grown: ParseResult[Result] = recursions.map((recursive: RecursiveParseResult[Input, Result, Result]) => {
            val results = recursive.get(singleResult(prev))
            results.flatMapReady(
              ready => if (ready.remainder.offset > prev.remainder.offset) singleResult(ready) else SREmpty.empty,
              uniform = false) // TODO maybe set this to uniform = true
          }).reduce((a,b) => a.merge(b))
          grow(recursions, grown, initialResults)
        }
      }, uniform = false)) // The uniform = false here is because applying recursion is similar to a Sequence
    }

    def getPreviousResult(input: Input, state: ParseState): Option[ParseResult[Result]] = {
      if (state.input == input && state.parsers.contains(parser))
          Some(RecursiveResults(Map(parser -> List(RecursiveParseResult[Input, Result, Result](x => x))), SREmpty.empty))
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
  case class RecursiveResults[+Result](recursions: Map[BuiltParser[Any], List[RecursiveParseResult[Input, _, Result]]], tail: ParseResults[Input, Result])
    extends ParseResults[Input, Result] {

    override def nonEmpty = false

    override def pop() = throw new Exception("Can't pop recursions")

    override def toList = tail.toList

    override def tailDepth = 0

    override def merge[Other >: Result](other: ParseResults[Input, Other], depth: Int,
                                        bests: Map[Input, Double] = Map.empty): RecursiveResults[Other] = other match {
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

    override def flatMap[NewResult](f: LazyParseResult[Input, Result] => ParseResults[Input, NewResult], uniform: Boolean) = {
      RecursiveResults(
        recursions.view.mapValues(s => s.map(r => r.compose(pr => pr.flatMap(f, uniform)))).toMap,
        tail.flatMap(f, uniform))
    }

    override def mapWithHistory[NewResult](f: ReadyParseResult[Input, Result] => ReadyParseResult[Input, NewResult],
                                           oldHistory: History[Input]) = {
      if (oldHistory.flawed)
        tail.mapWithHistory(f, oldHistory)
      else
        super.mapWithHistory(f, oldHistory)
    }
  }

  def moveState(input: Input, state: FixPointState) = if (state.input == input) state else FixPointState(input, Set.empty)
  class CheckCache[Result](parser: BuiltParser[Result]) extends CacheLike[Result] {
    // TODO I can differentiate between recursive and non-recursive results. Only the former depend on the state.
    val cache = mutable.HashMap[(Input, Set[BuiltParser[Any]]), ParseResult[Result]]()

    def apply(input: Input, state: ParseState): ParseResult[Result] = {
      val newState = moveState(input, state)
      val key = (input, newState.parsers)
      cache.get(key) match {
        case Some(value) =>
          value
        case _ =>
          val value: ParseResult[Result] = parser(input, newState)
          cache.put(key, value)
          value
      }
    }

    override def clear(): Unit = cache.clear()
  }
}
