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


  override def newParseState(input: Input) = FindFixPoint(input, Map.empty)

  trait HasDetectFixPoint[Result] {
    def parser: Parse[Result]

    def getPreviousResult(input: Input, state: FixPointState): Option[ParseResult[Result]] = {
      state match {
        case found: FoundFixPoint =>
          if (found.parse == parser && input == found.input)
            Some(found.intermediate.asInstanceOf[ParseResult[Result]])
          else
            None
        case find: FindFixPoint =>
          find.callStack.get(parser) match {
            case Some(switch) if find.input == input =>
              switch.foundRecursion = true
              Some(SREmpty)
            case _ => None
          }
      }
    }

    def growResult(input: Input, previous: ReadyParseResult[Result]): ParseResult[Result] = {

      val nextResult: ParseResult[Result] = parser(input, FoundFixPoint(input, parser, singleResult(previous)))
      nextResult.flatMapReady(ready => {
        if (ready.remainder.offset > previous.remainder.offset)
          growResult(input, ready)
        else
          singleResult(previous)
      })
    }
  }

  trait FixPointState {

  }

  case class FindFixPoint(input: Input, callStack: Map[Parse[Any], FoundRecursionSwitch]) extends FixPointState

  case class FoundFixPoint(input: Input, parse: Parse[Any], intermediate: ParseResult[Any]) extends FixPointState

  class FoundRecursionSwitch {
    var foundRecursion: Boolean = false
  }

  class DetectFixPoint[Result](val parser: Parse[Result]) extends HasDetectFixPoint[Result] with Parse[Result] {

    override def apply(input: Input, state: ParseState) = {
      getPreviousResult(input, state) match {
        case None =>

          val switch = new FoundRecursionSwitch()
          val newState = state match {
            case find: FindFixPoint =>
              if (input == find.input)
                FindFixPoint(input, find.callStack + (parser -> switch))
              else
                FindFixPoint(input, Map(parser -> switch))
            case found: FoundFixPoint => found
          }
          var result = parser(input, newState)
          result.flatMapReady(ready => {
            if (switch.foundRecursion) {
              growResult(input, ready)
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
