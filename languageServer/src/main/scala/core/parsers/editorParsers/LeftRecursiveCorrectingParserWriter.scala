package core.parsers.editorParsers

import scala.collection.mutable

trait LeftRecursiveCorrectingParserWriter extends CorrectingParserWriter {

  class CheckCache[Result](parser: Parse[Result]) extends Parse[Result] {

    val cache = mutable.HashMap[Input, ParseResult[Result]]()

    def apply(input: Input, state: ParseState): ParseResult[Result] = {
      cache.get (input) match {
        case Some(value) =>
          value
        case _ =>
          val detector = new FindFixPoint()
          val newState = if (state.input == input) {
            if (state.parsers.contains(parser))
              throw new Exception("")
            FixPointState(input, parser :: state.callStack, state.parsers + (parser -> detector))
          } else {
            state
          }
          val value: ParseResult[Result] = parser(input, newState)
          value.mapReady(ready => {
            if (!detector.partOfCycle)
              cache.put(input, value)
            ready
          })
      }
    }
  }

  type ParseState = FixPointState

  override def newParseState(input: Input) = FixPointState(input, List.empty, Map.empty)

  trait HasDetectFixPoint[Result] {
    def parser: Parse[Result]

    def getPreviousResult(input: Input, state: ParseState): Option[ParseResult[Result]] = {
      state.parsers.get(parser) match {
        case Some(innerState) if state.input == input =>
          val parts = state.callStack.takeWhile(p => p != parser)
          parts.foreach(part => state.parsers(part) match {
            case find: FindFixPoint => find.partOfCycle = true
            case _ =>
          })
          innerState match {
            case found: FoundFixPoint =>
              Some(found.intermediate.asInstanceOf[ParseResult[Result]])
            case find: FindFixPoint =>
              find.foundRecursion = true
              Some(SREmpty)
          }
        case _ => None
      }
    }

    def growResult(input: Input, state: ParseState, previous: ReadyParseResult[Result]): ParseResult[Result] = {
      val newState = FixPointState(input, state.callStack, state.parsers + (parser -> FoundFixPoint(singleResult(previous))))
      val nextResult: ParseResult[Result] = parser(input, newState)
      nextResult.flatMapReady(ready => {
        if (ready.remainder.offset > previous.remainder.offset)
          growResult(input, newState, ready)
        else
          singleResult(previous)
      })
    }
  }

  trait ParserState
  case class FixPointState(input: Input, callStack: List[Parse[Any]], parsers: Map[Parse[Any], ParserState])
  class FindFixPoint(var foundRecursion: Boolean = false, var partOfCycle: Boolean = false) extends ParserState
  case class FoundFixPoint(intermediate: ParseResult[Any]) extends ParserState

  class DetectFixPoint[Result](val parser: Parse[Result]) extends HasDetectFixPoint[Result] with Parse[Result] {

    override def apply(input: Input, state: ParseState) = {
      getPreviousResult(input, state) match {
        case None =>

          val detector = new FindFixPoint()
          val newState = if (state.input == input) {
            if (state.parsers.contains(parser))
              throw new Exception("")
            FixPointState(input, parser :: state.callStack, state.parsers + (parser -> detector))
          } else {
            FixPointState(input, List(parser), Map(parser -> detector))
          }
          val result = parser(input, newState)
          result.flatMapReady(ready => {
            if (detector.foundRecursion)
              growResult(input, newState, ready)
            else
              singleResult(ready)
          })

        case Some(result) => result
      }
    }
  }

  case class DetectFixPointAndCache[Result](parser: Parse[Result]) extends CheckCache(parser)
    with HasDetectFixPoint[Result] {

    override def apply(input: Input, state: ParseState): ParseResult[Result] = {

      cache.get(input) match {
        case Some(value) =>
          value
        case _ =>

          getPreviousResult(input, state) match {
            case Some(intermediate) =>
              intermediate

            case None =>

              val detector = new FindFixPoint()
              val newState = if (state.input == input) {
                if (state.parsers.contains(parser))
                  throw new Exception("")
                FixPointState(input, parser :: state.callStack, state.parsers + (parser -> detector))
              } else {
                FixPointState(input, List(parser), Map(parser -> detector))
              }
              val result = parser(input, newState)
              result.flatMapReady(ready => {
                val fullyGrown = if (detector.foundRecursion)
                  growResult(input, newState, ready)
                else
                  singleResult(ready)
                if (!detector.partOfCycle)
                  cache.put(input, result)
                fullyGrown
              })
          }
      }
    }
  }

  override def wrapParse[Result](parser: Parse[Result],
                                 shouldCache: Boolean,
                                 shouldDetectLeftRecursion: Boolean): Parse[Result] = {
      if (!shouldCache && !shouldDetectLeftRecursion) {
        return parser
      }
      if (shouldCache && shouldDetectLeftRecursion) {
        //return parser
        return new DetectFixPointAndCache[Result](parser)
      }
      if (shouldCache) {
        return new CheckCache[Result](parser)
      }

      new DetectFixPoint[Result](parser)
  }
}
