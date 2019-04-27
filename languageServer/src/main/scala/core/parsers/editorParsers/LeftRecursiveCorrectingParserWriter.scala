package core.parsers.editorParsers

import scala.collection.mutable

trait LeftRecursiveCorrectingParserWriter extends CorrectingParserWriter {

//  class CheckCache[Result](parseState: LeftRecursionDetectorState, parser: Parse[Result])
//    extends ParserState[Result](parseState, parser)
//      with Parse[Result] {
//
//    val cache = mutable.HashMap[Input, ParseResult[Result]]()
//
//    def apply(input: Input): ParseResult[Result] = {
//      if (isPartOfACycle) {
//        return parser(input)
//      }
//
//      cache.get (input) match {
//        case Some(value) => value
//        case _ =>
//          parseState.callStack.push(parser)
//          val value: ParseResult[Result] = parser(input)
//          parseState.callStack.pop()
//          if (!isPartOfACycle) {
//            cache.put (input, value)
//          }
//          value
//      }
//    }
//  }

  type ParseState = FixPointState


  override def newParseState(input: Input) = FixPointState(input, Map.empty)

  trait HasDetectFixPoint[Result] {
    def parser: Parse[Result]

    def getPreviousResult(input: Input, state: ParseState): Option[ParseResult[Result]] = {
      state.parsers.get(parser) match {
        case Some(innerState) if state.input == input =>
          innerState match {
            case found: FoundFixPoint =>
              Some(found.intermediate.asInstanceOf[ParseResult[Result]])
            case find: FindFixPoint =>
              find.switch.foundRecursion = true
              Some(SREmpty)
          }
        case _ => None
      }
    }

    def growResult(input: Input, state: ParseState, previous: ReadyParseResult[Result]): ParseResult[Result] = {

      val nextResult: ParseResult[Result] = parser(input, FixPointState(input, state.parsers + (parser -> FoundFixPoint(singleResult(previous)))))
      nextResult.flatMapReady(ready => {
        if (ready.remainder.offset > previous.remainder.offset)
          growResult(input, state, ready)
        else
          singleResult(previous)
      })
    }
  }

  trait ParserState
  case class FixPointState(input: Input, parsers: Map[Parse[Any], ParserState])
  case class FindFixPoint(switch: FoundRecursionSwitch) extends ParserState
  case class FoundFixPoint(intermediate: ParseResult[Any]) extends ParserState

  class FoundRecursionSwitch {
    var foundRecursion: Boolean = false
  }

  class DetectFixPoint[Result](val parser: Parse[Result]) extends HasDetectFixPoint[Result] with Parse[Result] {

    override def apply(input: Input, state: ParseState) = {
      getPreviousResult(input, state) match {
        case None =>

          val switch = new FoundRecursionSwitch()
          val newState = if (state.input == input) {
            FixPointState(input, state.parsers + (parser -> FindFixPoint(switch)))
          } else {
            FixPointState(input, Map(parser -> FindFixPoint(switch)))
          }
          val result = parser(input, newState)
          result.flatMapReady(ready => {
            if (switch.foundRecursion) {
              growResult(input, newState, ready)
            } else
              singleResult(ready)
          })

        case Some(result) => result
      }
    }
  }

//  class DetectFixPointAndCache[Result](parser: Parse[Result])
//    extends CheckCache(parseState, parser) with HasDetectFixPoint[Result] {
//
//    override def apply(input: Input, errorAllowance: Int): ParseResult[Result] = {
//      //      if (isPartOfACycle) {
//      //        return parser(input, errorAllowance)
//      //      }
//
//      cache.get(input) match {
//        case Some(value) => value
//        case _ =>
//
//          val value = getPreviousResult(input) match {
//            case None =>
//
//              callStackSet.add(input)
//              parseState.callStack.push(parser)
//              var result = parser(input, errorAllowance)
//              if (result.successful && hasBackEdge) {
//                result = growResult(input, errorAllowance, result)
//              }
//              callStackSet.remove(input)
//              parseState.callStack.pop()
//              result
//
//            case Some(result) => result
//          }
//
//          if (!isPartOfACycle) {
//            cache.put (input, value)
//          }
//          value
//      }
//    }
//  }

  override def wrapParse[Result](parser: Parse[Result],
                                 shouldCache: Boolean,
                                 shouldDetectLeftRecursion: Boolean): Parse[Result] = {
      if (!shouldCache && !shouldDetectLeftRecursion) {
        return parser
      }
      if (shouldCache && shouldDetectLeftRecursion) {
        new DetectFixPoint[Result](parser)
        //return parseState.parserStates.getOrElseUpdate(parser, new DetectFixPointAndCache[Any](parseState, parser)).asInstanceOf[Parse[Result]]
      }

      if (shouldCache) {
        //return parseState.parserStates.getOrElseUpdate(parser, new CheckCache[Any](parseState, parser)).asInstanceOf[Parse[Result]]
      }

      new DetectFixPoint[Result](parser)
  }
}
