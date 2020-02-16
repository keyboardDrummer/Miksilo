package core.parsers.editorParsers

import core.parsers.core.ParseText

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

  case class DetectFixPointAndCache[Result](text: ParseText,
                                            parser: BuiltParser[Result])
    extends CheckCache[Result](text, parser) {

    override def apply(input: Input, state: ParseState): ParseResult[Result] = {
      val key = (input, state)
      cache.get(key) match {
        case Some(value) =>
          value
        case None =>
          getPreviousResult(input, state) match {
            case Some(intermediate) =>
              intermediate

            case None =>

              val newState = if (state.input == input) {
                  if (state.parsers.contains(parser))
                    throw new Exception("recursion should have been detected.")
                FixPointState(input, state.parsers + parser)
              } else {
                FixPointState(input, Set(parser))
              }
              val initialResult = parser(input, newState)

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

  override def wrapParser[Result](text: ParseText,
                                  parser: BuiltParser[Result],
                                  shouldCache: Boolean,
                                  shouldDetectLeftRecursion: Boolean): BuiltParser[Result] = {
      if (!shouldCache && !shouldDetectLeftRecursion) {
        return parser
      }
      if (shouldDetectLeftRecursion) {
        return new DetectFixPointAndCache[Result](text, parser)
      }
      new CheckCache[Result](text, parser)
  }

  // TODO: replace List with something that has constant concat operation.
  case class RecursiveResults[+Result](recursions: Map[BuiltParser[Any], List[RecursiveParseResult[Input, _, Result]]],
                                       tail: ParseResults[Input, Result])
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

    override def latestRemainder = tail.latestRemainder

    override def move(array: ParseText, offset: Int) = throw new Exception("cannot move Recursive results")
  }

  class CheckCache[Result](text: ParseText, parser: BuiltParser[Result]) extends CacheLike[Result] {
    // TODO I can differentiate between recursive and non-recursive results. Only the former depend on the state.
    val cache = mutable.HashMap[(Input, ParseState), ParseResult[Result]]()

    def apply(input: Input, state: ParseState): ParseResult[Result] = {
      val key = (input, state)
      cache.get(key) match {
        case Some(value) =>
          value
        case _ =>
          val value: ParseResult[Result] = parser(input, state)
          cache.put(key, value)
          value
      }
    }

    override def clear(): Unit = cache.clear()

    override def change(from: Int, until: Int, inserted: Int): Unit = {

      val entries = cache.toList

      val insertionLength = inserted - (until - from)
      def updateInput(input: Input): Input = {
        if (input.offset >= from) {
          input.drop(text, insertionLength)
        } else {
          input
        }
      }
      for(entry <- entries) {
        val entryStart = entry._1._1.offset
        val entryEnd = Math.max(entryStart + 1, entry._2.latestRemainder)
        val entryIntersectsWithRemoval = from < entryEnd && entryStart < until // entryStart < from && from < entryEnd
        if (entryIntersectsWithRemoval) {
          cache.remove(entry._1)
        } else {
          if (entryStart >= from) {
            cache.remove(entry._1)
            val newKey = (updateInput(entry._1._1), FixPointState(updateInput(entry._1._2.input), entry._1._2.parsers))
            val newValue = entry._2.move(text, insertionLength)
            cache.put(newKey, newValue)
          }
        }
      }
    }
  }
}
