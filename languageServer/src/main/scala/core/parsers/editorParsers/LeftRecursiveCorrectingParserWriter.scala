package core.parsers.editorParsers

import scala.collection.mutable

trait LeftRecursiveCorrectingParserWriter extends CorrectingParserWriter {

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

  trait HasDetectFixPoint[Result] {
    def parser: Parse[Result]

    def getPreviousResult(input: Input, state: ParseState): Option[ParseResult[Result]] = {
      state.parsers.get(parser) match {
        case Some(innerState) if state.input == input =>
          val parts = state.callStack.takeWhile(p => p != parser)
          parts.foreach(part => state.isCycle(part) match {
            case find: IsPartOfCycle => find.partOfCycle = true
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

    //During growing, the input can change, and then state.parsers is flushed, and fixpoint detection.
    //Loop detector -> input increase -> CheckCache -> Loop detector -> CheckCache (boem)
    //During the growing, the cache recursion detection fails???
    def growResult(input: Input, state: ParseState, previous: ReadyParseResult[Result]): ParseResult[Result] = {
      val newState = FixPointState(input, state.callStack,
        state.parsers + (parser -> FoundFixPoint(singleResult(previous))), state.isCycle)

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
  class IsPartOfCycle(var partOfCycle: Boolean = false)
  case class FixPointState(input: Input, callStack: List[Parse[Any]],
                           parsers: Map[Parse[Any], ParserState],
                           isCycle: Map[Parse[Any], IsPartOfCycle])
  class FindFixPoint(var foundRecursion: Boolean = false) extends ParserState
  case class FoundFixPoint(intermediate: ParseResult[Any]) extends ParserState

  class DetectFixPoint[Result](val parser: Parse[Result]) extends HasDetectFixPoint[Result] with Parse[Result] {

    override def apply(input: Input, state: ParseState) = {
      getPreviousResult(input, state) match {
        case None =>

          val detector = new FindFixPoint()
          val newState = if (state.input == input) {
//            if (state.parsers.contains(parser))
//              throw new Exception("")
            FixPointState(input, parser :: state.callStack, state.parsers + (parser -> detector), state.isCycle)
          } else {
            FixPointState(input, List(parser), Map(parser -> detector), Map.empty)
          }
          val result = parser(input, newState)
          val grownResult =
            if (detector.foundRecursion)
              result.flatMapReady(ready => growResult(input, newState, ready))
            else
              result
          grownResult
        case Some(result) =>
          result
      }
    }
  }

  case class DetectFixPointAndCache[Result](parser: Parse[Result]) extends CheckCache(parser)
    with HasDetectFixPoint[Result] {

    override def apply(input: Input, state: ParseState): ParseResult[Result] = {

      getPreviousResult(input, state) match {
        case Some(intermediate) =>
          intermediate

        case None =>

          cache.get(input) match {
            case Some(value) =>
              value
            case _ =>

              val detector = new FindFixPoint()
              val newState = if (state.input == input) {
//                if (state.parsers.contains(parser))
//                  throw new Exception("recursion should have been detected.")
                FixPointState(input, parser :: state.callStack, state.parsers + (parser -> detector), state.isCycle + (parser -> isCycle))
              } else {
                FixPointState(input, List(parser), Map(parser -> detector), Map(parser -> isCycle))
              }
              val result = parser(input, newState)

              // I believe any left recursion will be detected immediately, if it exists, since the | operator always calls into both children.
              val grownResult =
                if (detector.foundRecursion)
                  result.flatMapReady(ready => growResult(input, newState, ready))
                else result

              if (!isCycle.partOfCycle)
                cache.put(input, grownResult)

              grownResult
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
        return new DetectFixPointAndCache[Result](parser)
      }
      if (shouldCache) {
        return new CheckCache[Result](parser)
      }

      new DetectFixPoint[Result](parser)
  }
}
