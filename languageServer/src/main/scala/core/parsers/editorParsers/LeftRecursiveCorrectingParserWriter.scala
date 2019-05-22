package core.parsers.editorParsers

import core.language.node.SourceRange
import deltas.expression.ExpressionDelta

import scala.collection.mutable

trait LeftRecursiveCorrectingParserWriter extends CorrectingParserWriter {

  case class RecursiveParseResult[SeedResult, Result](input: Input,
                                                      parser: Parse[SeedResult],
                                                      get: ParseResult[SeedResult] => ParseResult[Result])
    extends LazyParseResult[Result] {

    def history = History.empty[Input]
    override val score = 1000000 + history.score

    override def toString = "Recursive: " + parser.debugName

    override def map[NewResult](f: Result => NewResult) = {
      RecursiveParseResult[SeedResult, NewResult](input, parser, r => get(r).map(f))
    }

    override def flatMapReady[NewResult](f: ReadyParseResult[Result] => SortedParseResults[NewResult]) = {
      singleResult(RecursiveParseResult[SeedResult, NewResult](input, parser, r => get(r).flatMapReady(f)))
    }

    override def mapReady[NewResult](f: ReadyParseResult[Result] => ReadyParseResult[NewResult]) =
      RecursiveParseResult[SeedResult, NewResult](input, parser, r => get(r).mapReady(f))

    override def mapWithHistory[NewResult](f: ReadyParseResult[Result] => ReadyParseResult[NewResult], oldHistory: MyHistory) =
      if (oldHistory.flawed) {
        SREmpty
      }
      else if (oldHistory.score > 0) {
        System.out.append("what?")
        ???
      }
      else
        singleResult(RecursiveParseResult[SeedResult, NewResult](input, parser,
          r => get(r).mapWithHistory(f, oldHistory)))
  }

  class CheckCache[Result](parser: Parse[Result]) extends Parse[Result] {
// TODO I can differentiate between recursive and non-recursive results. Only the former depend on the state.
    val cache = mutable.HashMap[(Input, ParseState), ParseResult[Result]]()

    def apply(input: Input, state: ParseState): ParseResult[Result] = {
      val key = (input, state.key)
      cache.get(key) match {
        case Some(value) =>
          value
        case _ =>
          val value: ParseResult[Result] = parser(input, state)
          cache.put(key, value)
          value
      }
    }
  }

  type ParseState = FixPointState

  override def newParseState(input: Input) = FixPointState(input, Set.empty)

  case class FixPointState(input: Input, parsers: Set[Parse[Any]]) {
    def key = this
  }

  case class DetectFixPointAndCache[Result](parser: Parse[Result]) extends CheckCache[Result](parser) {

    override def apply(input: Input, state: ParseState): ParseResult[Result] = {
      val key = (input, state.key)
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
              val newState = if (state.input == input) {
                  if (state.parsers.contains(parser))
                    throw new Exception("recursion should have been detected.")
                FixPointState(input, state.parsers + parser)
              } else {
                FixPointState(input, Set(parser))
              }
              val initialResult = parser(input, newState)

              var foundRecursion = false //true
              var left = initialResult
              var continue = true
              while(continue) {
                if (!left.isInstanceOf[SRCons[Result]]) {
                  continue = false
                } else {
                  val cons = left.asInstanceOf[SRCons[Result]]
                  cons.head match {
                    case recursive: RecursiveParseResult[Result, Result] =>
                      if (recursive.parser == parser)
                        foundRecursion = true
                    case _ =>
                      continue = false
                  }
                  left = cons.tail
                }
              }

              val resultWithoutRecursion: ParseResult[Result] = initialResult.flatMap({
                case recursive: RecursiveParseResult[Result, Result] =>
                  if (recursive.parser == parser) {
                    //foundRecursion = true
                    SREmpty
                  }
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

              val result = if (foundRecursion)
                grow(resultWithoutRecursion, initialResult)
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
    def grow(previous: ParseResult[Result], initialResults: ParseResult[Result]): ParseResult[Result] = {
      // TODO Consider replacing the previous.merge by moving that inside the lambda.
      previous.merge(previous.flatMapReady(prev => {
        if (prev.history.flawed)
          SREmpty // TODO consider growing this as well
        else {
          val grown: ParseResult[Result] = initialResults.flatMap({
            case recursive: RecursiveParseResult[Result, Result] if recursive.parser == parser =>
              val results = recursive.get(singleResult(prev))
              results.flatMapReady(r => if (r.remainder.offset > prev.remainder.offset) singleResult(r) else SREmpty)
            case _ => SREmpty
          })
          grow(grown, initialResults)
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
      if (state.input == input && state.parsers.contains(parser))
          Some(singleResult(RecursiveParseResult[Result, Result](input, parser, x => x)))
      else
        None
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
