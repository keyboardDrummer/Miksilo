package core.parsers.editorParsers

import core.language.node.SourceRange
import deltas.expression.ExpressionDelta

import scala.collection.mutable

trait LeftRecursiveCorrectingParserWriter extends CorrectingParserWriter {

  var outerCounter = 0
  case class RecursiveParseResult[SeedResult, Result](input: Input,
                                                      detector: FindFixPoint,
                                                      var counter: Int = 0,
                                                      state: ParseState,
                                                      parser: Parse[SeedResult],
                                                      get: ParseResult[SeedResult] => ParseResult[Result])
    extends LazyParseResult[Result] {

    if (counter == 2) {
      System.out.append("")
    }

    if (input.offset == 1 && parser.debugName == ExpressionDelta.FirstPrecedenceGrammar) {
      System.out.append("")
      if (counter == 0) {
        counter = outerCounter
        if (counter == 2) {
          System.out.append("")
        }
        outerCounter += 1
      }
    }

    def history = History.empty[Input]

    override def toString = "Recursive: " + state.callStack

    override def map[NewResult](f: Result => NewResult) = {
      RecursiveParseResult[SeedResult, NewResult](input, detector, counter, state, parser, r => get(r).map(f))
    }

    override def flatMapReady[NewResult](f: ReadyParseResult[Result] => SortedParseResults[NewResult]) = {
      singleResult(RecursiveParseResult[SeedResult, NewResult](input, detector, counter, state, parser, r => {
        get(r).flatMapReady(ready => {
          if (ready.remainder == input) {
            System.out.append("bug")
            ???
          }
          f(ready)
        })
      }))
    } // TODO misschien hier checken dat r.remainder != input

    override def mapReady[NewResult](f: ReadyParseResult[Result] => ReadyParseResult[NewResult]) =
      RecursiveParseResult[SeedResult, NewResult](input, detector, counter, state, parser, r => get(r).mapReady(f))

    override def mapWithHistory[NewResult](f: ReadyParseResult[Result] => ReadyParseResult[NewResult], oldHistory: MyHistory) =
      if (oldHistory.flawed) {
        new DelayedParseResult[NewResult](History.error(new ParseError[Input] {
          override def penalty = 1000000

          override def message = "blerp"

          override def from = ???

          override def to = ???

          override def range = ???
        }), () => SREmpty)
      }
      else if (oldHistory.score > 0) {
        System.out.append("what?")
        ???
      }
      else
        RecursiveParseResult[SeedResult, NewResult](input, detector, counter, state, parser, r => get(r).mapWithHistory(f, oldHistory))
  }

  class CheckCache[Result](parser: Parse[Result]) extends Parse[Result] {

    val cache = mutable.HashMap[(Input, ParseState), ParseResult[Result]]()

    def apply(input: Input, state: ParseState): ParseResult[Result] = {
      val key = (input, state)
      cache.get(key) match {
        case Some(value) =>
          value
        case _ =>
          val value: ParseResult[Result] = parser(input, state)
          if (!cache.contains(key)) { // TODO check kan weg.
            cache.put(key, value)
          }

          value
      }
    }
  }

  type ParseState = FixPointState

  override def newParseState(input: Input) = FixPointState(input, List.empty, Map.empty)

  case class FixPointState(input: Input, callStack: List[Parse[Any]], parsers: Map[Parse[Any], FindFixPoint])
  class FindFixPoint(var state: ParseState = null, var foundRecursion: Boolean = false)

  case class DetectFixPointAndCache[Result](parser: Parse[Result]) extends CheckCache[Result](parser) {

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

              // the recursion detection moet gebeuren in een path die niet terugkomt bij de recursion fixpoint.
              // misschien dat nested fixpoints van dezelfde parser er wat mee te maken hebben.
              // heeft het zin om een history te hebben bij een recursiveResult? ik denk van niet
              // het gebeurt toch voordat er input is.
              // als er iets gedropped moet worden kan dan ook binnen de recursie in plaats van er voor
              // kan de recursie genest worden in een delayed result, en dan later niet meer gevonden?
              // stel je krijgt left een delayed, en rechts een recursie, dan is het resultaat een delayed die later een recursie oplevert.

              //kan een recursie schuilen in een andere recursie???
              val detector: FindFixPoint = new FindFixPoint()
              val newState = if (state.input == input) {
                  if (state.parsers.contains(parser))
                    throw new Exception("recursion should have been detected.")
                FixPointState(input, parser :: state.callStack, state.parsers + (parser -> detector))
              } else {
                FixPointState(input, List(parser), Map(parser -> detector))
              }
              detector.state = newState
              val initialResult = parser(input, newState)

              val resultWithoutRecursion: ParseResult[Result] = initialResult.flatMap({
                case recursive: RecursiveParseResult[Result, Result] =>
                  if (recursive.detector == detector)
                    SREmpty
                  else {
                    if (!newState.parsers.contains(recursive.parser)) {
                      System.out.append("bug")
                      ???
                    }
                    else
                      singleResult(recursive)
                  }
                case lazyResult => singleResult(lazyResult)
              })

              val result = if (detector.foundRecursion)
                grow(detector, resultWithoutRecursion, initialResult)
              else
                resultWithoutRecursion

              cache.put(key, result)
              result
          }
      }
    }

    //it can keep trying to grow the same shit indefinitely if there is a recursive loop that doesn't parse anything.
    /*
    Previous bevat ook recursions van andere fixpoints, die kunnen dus als seed gebruikt worden,
    echter zijn ze dan een soort delay.. wanneer gaan ze dan verder?
     */
    def grow(detector: FindFixPoint, previous: ParseResult[Result], initialResults: ParseResult[Result]): ParseResult[Result] = {
      // TODO Consider replacing the previous.merge by moving that inside the lambda.
      previous.merge(previous.flatMapReady(prev => {
        if (prev.history.flawed)
          SREmpty // TODO consider growing this as well
        else {
          val grown: ParseResult[Result] = initialResults.flatMap({
            case recursive: RecursiveParseResult[Result, Result] if recursive.detector == detector =>
              val results = recursive.get(singleResult(prev))
              results.flatMapReady(r => if (r.remainder.offset > prev.remainder.offset) singleResult(r) else SREmpty)
            case _ => SREmpty
          })
          grow(detector, grown, initialResults)
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
          Some(singleResult(RecursiveParseResult[Result, Result](input, find, 0, find.state, parser, x => x)))
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
