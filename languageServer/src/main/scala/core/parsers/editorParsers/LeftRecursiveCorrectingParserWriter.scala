package core.parsers.editorParsers

import scala.collection.mutable

trait LeftRecursiveCorrectingParserWriter extends CorrectingParserWriter {

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

              val newState = if (state.input == input) {
                  if (state.parsers.contains(parser))
                    throw new Exception("recursion should have been detected.")
                FixPointState(input, state.parsers + parser)
              } else {
                FixPointState(input, Set(parser))
              }
              val initialResult = parser(input, newState)

              val RecursionsList(recursions, resultWithoutRecursion) = initialResult.recursionsFor(parser)
              var foundRecursion = recursions.nonEmpty

              val result = if (foundRecursion)
                grow(recursions, resultWithoutRecursion, initialResult)
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
    def grow(recursions: List[RecursiveParseResult[Result, Result]], previous: ParseResult[Result], initialResults: ParseResult[Result]): ParseResult[Result] = {
      // TODO Consider replacing the previous.merge by moving that inside the lambda.
      previous.merge(previous.flatMapReady(prev => {
        if (prev.history.flawed)
          SREmpty // TODO consider growing this as well
        else {
          val grown: ParseResult[Result] = recursions.map((recursive: RecursiveParseResult[Result, Result]) => {
            val results = recursive.get(singleResult(prev))
            results.flatMapReady(
              ready => if (ready.remainder.offset > prev.remainder.offset) singleResult(ready) else SREmpty,
              uniform = false) // TODO maybe set this to uniform = true
          }).reduce((a,b) => a.merge(b))
          grow(recursions, grown, initialResults)
        }
      }, uniform = false)) // The uniform = false here is because applying recursion is similar to a Sequence
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
