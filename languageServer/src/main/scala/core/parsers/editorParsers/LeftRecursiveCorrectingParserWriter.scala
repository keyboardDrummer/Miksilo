package core.parsers.editorParsers

import core.language.node.SourceRange

import scala.collection.mutable

trait LeftRecursiveCorrectingParserWriter extends CorrectingParserWriter {

  case class RecursiveParseResult[SeedResult, Result](parser: Parse[SeedResult],
                                          history: MyHistory,
                                          get: ParseResult[SeedResult] => ParseResult[Result]) extends LazyParseResult[Result] {

    override def map[NewResult](f: Result => NewResult) = {
      RecursiveParseResult[SeedResult, NewResult](parser, history, r => get(r).map(f))
    }

    override def flatMapReady[NewResult](f: ReadyParseResult[Result] => SortedParseResults[NewResult]) = {
      singleResult(RecursiveParseResult[SeedResult, NewResult](parser, history, r => get(r).flatMapReady(f)))
    }

    override def mapReady[NewResult](f: ReadyParseResult[Result] => ReadyParseResult[NewResult]) =
      RecursiveParseResult[SeedResult, NewResult](parser, history, r => get(r).mapReady(f))

    override def mapWithHistory[NewResult](f: ReadyParseResult[Result] => ReadyParseResult[NewResult], oldHistory: MyHistory) =
      RecursiveParseResult[SeedResult, NewResult](parser, history ++ oldHistory, r => get(r).mapWithHistory(f, oldHistory))
  }

  class CheckCache[Result](parser: Parse[Result]) extends Parse[Result] {

    val cache = mutable.HashMap[Input, ParseResult[Result]]()

    val isCycle = new IsPartOfCycle()
    def apply(input: Input, state: ParseState): ParseResult[Result] = {
      cache.get (input) match {
        case Some(value) =>
          value
        case _ =>
          val newState = if (state.input == input) {
//            if (state.parsers.contains(parser))
//              throw new Exception("recursion should have been detected")
            FixPointState(input, parser :: state.callStack, state.parsers, state.isCycle + (parser -> isCycle))
          } else {
            FixPointState(input, List(parser), Map.empty, Map(parser -> isCycle))
          }
          val value: ParseResult[Result] = parser(input, newState)
          if (!isCycle.partOfCycle && !cache.contains(input)) {
            cache.put(input, value)
          }

          value
      }
    }
  }

  type ParseState = FixPointState

  override def newParseState(input: Input) = FixPointState(input, List.empty, Map.empty, Map.empty)

  class IsPartOfCycle(var partOfCycle: Boolean = false)
  case class FixPointState(input: Input, callStack: List[Parse[Any]],
                           parsers: Map[Parse[Any], FindFixPoint],
                           isCycle: Map[Parse[Any], IsPartOfCycle])
  class FindFixPoint(var foundRecursion: Boolean = false)

  case class DetectFixPointAndCache[Result](parser: Parse[Result]) extends Parse[Result] {


    override def apply(input: Input, state: ParseState): ParseResult[Result] = {

      getPreviousResult(input, state) match {
        case Some(intermediate) =>
          intermediate

        case None =>

          val detector = new FindFixPoint()
          val newState = if (state.input == input) {
//                if (state.parsers.contains(parser))
//                  throw new Exception("recursion should have been detected.")
            FixPointState(input, parser :: state.callStack, state.parsers + (parser -> detector), state.isCycle)
          } else {
            FixPointState(input, List(parser), Map(parser -> detector), Map.empty)
          }
          val resultWithRecursion = parser(input, newState)

//          val resolveRecursion: LazyParseResult[Result] => SortedParseResults[Result] = {
//            case recursive: RecursiveParseResult[Result] =>
//              if (recursive.parser == parser) {
//                recursive.get(endResult)
//              }
//              else {
//                singleResult(recursive)
//              }
//            case ready: ReadyParseResult[Result] => singleResult(ready)
//            case delayed: DelayedParseResult[Result] => singleResult(new DelayedParseResult(delayed.history, () => {
//              val intermediate = delayed.results
//              intermediate.flatMap(resolveRecursion)
//            }))
//          }
//          lazy val endResult = resultWithRecursion.flatMap(resolveRecursion)

          val resultsWithoutRecursion: ParseResult[Result] = resultWithRecursion.flatMap({
            case recursive: RecursiveParseResult[Result, Result] if recursive.parser == parser =>
              SREmpty
            case lazyResult => singleResult(lazyResult)
          })
          val meRecursions1: ParseResult[Result] = resultWithRecursion.flatMap({
            case recursive: RecursiveParseResult[Result, Result] if recursive.parser == parser =>
              recursive.get(resultsWithoutRecursion)
            case _ => SREmpty
          })
          val meRecursions2: ParseResult[Result] = resultWithRecursion.flatMap({
            case recursive: RecursiveParseResult[Result, Result] if recursive.parser == parser =>
              recursive.get(meRecursions1)
            case _ => SREmpty
          })
          val meRecursions3: ParseResult[Result] = resultWithRecursion.flatMap({
            case recursive: RecursiveParseResult[Result, Result] if recursive.parser == parser =>
              recursive.get(meRecursions2)
            case _ => SREmpty
          })

          val meRecursions4: ParseResult[Result] = resultWithRecursion.flatMap({
            case recursive: RecursiveParseResult[Result, Result] if recursive.parser == parser =>
              recursive.get(meRecursions3)
            case _ => SREmpty
          })

          val meRecursions5: ParseResult[Result] = resultWithRecursion.flatMap({
            case recursive: RecursiveParseResult[Result, Result] if recursive.parser == parser =>
              recursive.get(meRecursions4)
            case _ => SREmpty
          })

          resultsWithoutRecursion.merge(meRecursions1).merge(meRecursions2).merge(meRecursions3).merge(meRecursions4).merge(meRecursions5)
      }
    }

//    def grow(previous: ParseResult[Result], initialResults: ParseResult[Result]): ParseResult[Result] = {
//      val grown: ParseResult[Result] = initialResults.flatMap({
//        case recursive: RecursiveParseResult[Result, Result] if recursive.parser == parser =>
//          recursive.get(previous)
//        case _ => SREmpty
//      })
//      previous.merge(singleResult(new DelayedParseResult(History.empty[Input], () => grow(grown, initialResults))))
//    }

    case class RecursionError(input: Input) extends ParseError[Input] {
      override def penalty = 0.000001

      override def message = "recursion"

      override def from = input

      override def to = input

      override def range = SourceRange(from.position, to.position)
    }

    def getPreviousResult(input: Input, state: ParseState): Option[ParseResult[Result]] = {
      state.parsers.get(parser) match {
        case Some(find) if state.input == input =>
          val parts = state.callStack.takeWhile(p => p != parser)
          parts.foreach(part => state.isCycle.get(part) match {
            case Some(isCycle) => isCycle.partOfCycle = true
            case _ =>
          })
          find.foundRecursion = true
          val smallErrorHistory = SingleError(0, RecursionError(input))
          Some(singleResult(RecursiveParseResult[Result, Result](parser, History.empty[Input], x => x)))
        case _ => None
      }
    }
  }

  override def wrapParse[Result](parser: Parse[Result],
                                 shouldCache: Boolean,
                                 shouldDetectLeftRecursion: Boolean): Parse[Result] = {
      if (!shouldCache && !shouldDetectLeftRecursion) {
        return parser
      }
      if (shouldDetectLeftRecursion) {
        return new DetectFixPointAndCache[Result](parser)
      }
      new CheckCache[Result](parser)
  }
}
