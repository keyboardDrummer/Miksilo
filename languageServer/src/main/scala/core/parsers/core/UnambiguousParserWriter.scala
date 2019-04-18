package core.parsers.core

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds

trait UnambiguousParserWriter extends LeftRecursiveParserWriter {
  type ParseResult[+Result] <: UnambiguousParseResult[Result]

  trait UnambiguousParseResult[+Result] extends ParseResultLike[Result] {
    def getSuccessRemainder: Option[Input]
    def errorsRequiredForChange: Int
    override def successful: Boolean = getSuccessRemainder.nonEmpty
    def get: Result
  }

  case class CacheValue[Result](maxErrorAllowance: Int, result: ParseResult[Result])

  class CheckCache[Result](parseState: LeftRecursionDetectorState, parser: Parse[Result])
    extends ParserState[Result](parseState, parser)
    with Parse[Result] {

    val cache = mutable.HashMap[Input, CacheValue[Result]]()

    def apply(input: Input, errorAllowance: Int): ParseResult[Result] = {
      if (isPartOfACycle) {
        return parser(input, errorAllowance)
      }

      cache.get (input) match {
        case Some (value) if value.maxErrorAllowance >= errorAllowance => value.result
        case _ =>
          parseState.callStack.push(parser)
          val value: ParseResult[Result] = parser(input, errorAllowance)
          parseState.callStack.pop()
          if (!isPartOfACycle) {
            val maxErrors = if (value.errorsRequiredForChange == Int.MaxValue) value.errorsRequiredForChange else
              errorAllowance
            cache.put (input, CacheValue(maxErrors, value))
          }
          value
      }
    }
  }

  trait HasDetectFixPoint[Result] {
    def parseState: LeftRecursionDetectorState
    def parser: Parse[Result]

    val recursionIntermediates = mutable.HashMap[Input, ParseResult[Result]]()
    val callStackSet = mutable.HashSet[Input]() // TODO might not be needed if we put an abort in the intermediates.
    var hasBackEdge: Boolean = false

    def getPreviousResult(input: Input): Option[ParseResult[Result]] = {
      if (!callStackSet.contains(input))
        return None

      Some(recursionIntermediates.getOrElse(input, {
        hasBackEdge = true
        val index = parseState.callStack.indexOf(parser)
        parseState.callStack.take(index + 1).
          foreach(parser => parseState.parserStates(parser).isPartOfACycle = true) // TODO this would also be possible by returning a value that indicates we found a cycle, like the abort!
        abort
      }))
    }

    @tailrec
    final def growResult(input: Input, errorAllowance: Int, previous: ParseResult[Result]): ParseResult[Result] = {
      recursionIntermediates.put(input, previous)

      val nextResult: ParseResult[Result] = parser(input, errorAllowance)
      nextResult.getSuccessRemainder match {
        case Some(remainder) if remainder.offset > previous.getSuccessRemainder.get.offset =>
          growResult(input, errorAllowance, nextResult)
        case _ =>
          recursionIntermediates.remove(input)
          previous
      }
    }
  }

  class DetectFixPoint[Result](parseState: LeftRecursionDetectorState, parser: Parse[Result])
    extends ParserState[Result](parseState, parser) with HasDetectFixPoint[Result] with Parse[Result] {

    override def apply(input: Input, errorAllowance: Int) = {
      getPreviousResult(input) match {
        case None =>

          callStackSet.add(input)
          parseState.callStack.push(parser)
          var result = parser(input, errorAllowance)
          if (result.successful && hasBackEdge) {
            result = growResult(input, errorAllowance, result)
          }
          callStackSet.remove(input)
          parseState.callStack.pop()
          result

        case Some(result) => result
      }
    }
  }

  class DetectFixPointAndCache[Result](parseState: LeftRecursionDetectorState, parser: Parse[Result])
    extends CheckCache(parseState, parser) with HasDetectFixPoint[Result] {

    override def apply(input: Input, errorAllowance: Int): ParseResult[Result] = {
//      if (isPartOfACycle) {
//        return parser(input, errorAllowance)
//      }

      cache.get(input) match {
        case Some (value) if value.maxErrorAllowance >= errorAllowance => value.result
        case _ =>

          val value = getPreviousResult(input) match {
            case None =>

              callStackSet.add(input)
              parseState.callStack.push(parser)
              var result = parser(input, errorAllowance)
              if (result.successful && hasBackEdge) {
                result = growResult(input, errorAllowance, result)
              }
              callStackSet.remove(input)
              parseState.callStack.pop()
              result

            case Some(result) => result
          }

          if (!isPartOfACycle) {
            val maxErrors = if (value.errorsRequiredForChange == Int.MaxValue) value.errorsRequiredForChange else
              errorAllowance
            cache.put (input, CacheValue(maxErrors,value))
          }
          value
      }
    }
  }

  class ParserState[Result](val parseState: LeftRecursionDetectorState, val parser: Parse[Result]) {
    var isPartOfACycle: Boolean = false // TODO investigate whether it could be useful to have this property switch back and forth, instead of only switch once.
  }

  override def wrapParse[Result](parseState: ParseState,
                                 parser: Parse[Result],
                                 shouldCache: Boolean,
                                 shouldDetectLeftRecursion: Boolean): Parse[Result] = {
    if (!shouldCache && !shouldDetectLeftRecursion) {
      return parser
    }
    if (shouldCache && shouldDetectLeftRecursion) {
      return parseState.parserStates.getOrElseUpdate(parser, new DetectFixPointAndCache[Any](parseState, parser)).asInstanceOf[Parse[Result]]
    }

    if (shouldCache) {
      return parseState.parserStates.getOrElseUpdate(parser, new CheckCache[Any](parseState, parser)).asInstanceOf[Parse[Result]]
    }

    parseState.parserStates.getOrElseUpdate(parser, new DetectFixPoint[Any](parseState, parser)).asInstanceOf[Parse[Result]]
  }

  type ParseState = LeftRecursionDetectorState
  class LeftRecursionDetectorState {
    val parserStates = mutable.HashMap[Parse[Any], ParserState[Any]]()
    val callStack = mutable.Stack[Parse[Any]]()
    var maxErrors = 0
  }
}
