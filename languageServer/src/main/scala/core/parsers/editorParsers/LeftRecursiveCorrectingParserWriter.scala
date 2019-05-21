package core.parsers.editorParsers

import core.language.node.SourceRange
import deltas.expression.ExpressionDelta

import scala.collection.mutable

trait LeftRecursiveCorrectingParserWriter extends CorrectingParserWriter {

  case class RecursiveParseResult[SeedResult, Result](state: ParseState, parser: Parse[SeedResult],
                                          history: MyHistory,
                                          get: ParseResult[SeedResult] => ParseResult[Result]) extends LazyParseResult[Result] {


    override def toString = "Recursive: " + state.callStack

    override def map[NewResult](f: Result => NewResult) = {
      RecursiveParseResult[SeedResult, NewResult](state, parser, history, r => get(r).map(f))
    }

    override def flatMapReady[NewResult](f: ReadyParseResult[Result] => SortedParseResults[NewResult]) = {
      singleResult(RecursiveParseResult[SeedResult, NewResult](state, parser, history, r => get(r).flatMapReady(f)))
    }

    override def mapReady[NewResult](f: ReadyParseResult[Result] => ReadyParseResult[NewResult]) =
      RecursiveParseResult[SeedResult, NewResult](state, parser, history, r => get(r).mapReady(f))

    override def mapWithHistory[NewResult](f: ReadyParseResult[Result] => ReadyParseResult[NewResult], oldHistory: MyHistory) =
      RecursiveParseResult[SeedResult, NewResult](state, parser, history ++ oldHistory, r => get(r).mapWithHistory(f, oldHistory))
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
          if (!cache.contains(input)) {
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
  class FindFixPoint(val state: ParseState, var foundRecursion: Boolean = false)

  case class DetectFixPointAndCache[Result](parser: Parse[Result]) extends CheckCache[Result](parser) {

    override def apply(input: Input, state: ParseState): ParseResult[Result] = {

      cache.get (input) match {
        case Some(value) =>
          value
        case None =>
          getPreviousResult(input, state) match {
            case Some(intermediate) =>
              intermediate

            case None =>

              // the recursion detection moet gebeuren in een path die niet terugkomt bij de recursion fixpoint.
              // misschien dat nested fixpoints van dezelfde parser er wat mee te maken hebben.
              // heeft het zin om een history te hebben bij een recursiveResult? ik denk van niet
              // het gebeurt toch voordat er input is.
              // als er iets gedropped moet worden kan dan ook binnen de recursie in plaats van er voor
              // kan de recursie genest worden in een delayed result, en dan later niet meer gevonden?
              // stel je krijgt left een delayed, en rechts een recursie, dan is het resultaat een delayed die later een recursie oplevert.
              val detector = new FindFixPoint(state)
              val newState = if (state.input == input) {
                //                if (state.parsers.contains(parser))
                //                  throw new Exception("recursion should have been detected.")
                FixPointState(input, parser :: state.callStack, state.parsers + (parser -> detector), state.isCycle)
              } else {
                FixPointState(input, List(parser), Map(parser -> detector), Map.empty)
              }
              val initialResult = parser(input, newState)

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

              val resultWithoutRecursion: ParseResult[Result] = initialResult.flatMap({
                case recursive: RecursiveParseResult[Result, Result] if recursive.state == state =>
                  SREmpty
                case lazyResult => singleResult(lazyResult)
              })

              val result = if (detector.foundRecursion)
                grow(state, resultWithoutRecursion, initialResult)
              else
                resultWithoutRecursion

              cache.put(input, result)
              result
          }
      }
    }

    //it can keep trying to grow the same shit indefinitely if there is a recursive loop that doesn't parse anything.
    /*
    Previous bevat ook recursions van andere fixpoints, die kunnen dus als seed gebruikt worden,
    echter zijn ze dan een soort delay.. wanneer gaan ze dan verder?
     */
    def grow(state: ParseState, previous: ParseResult[Result], initialResults: ParseResult[Result]): ParseResult[Result] = {
      // TODO Consider replacing the previous.merge by moving that inside the lambda.
      previous.merge(previous.flatMapReady(prev => {
        if (prev.history.flawed)
          SREmpty // TODO consider growing this as well
        else {
          val grown: ParseResult[Result] = initialResults.flatMap({
            case recursive: RecursiveParseResult[Result, Result] if recursive.state == state =>
              val results = recursive.get(singleResult(prev))
              results.flatMapReady(r => if (r.remainder.offset > prev.remainder.offset) singleResult(r) else SREmpty)
            case _ => SREmpty
          })
          grow(state, grown, initialResults)
        }
      }))
    }

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
          find.foundRecursion = true
          Some(singleResult(RecursiveParseResult[Result, Result](find.state, parser, History.empty[Input], x => x)))
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
