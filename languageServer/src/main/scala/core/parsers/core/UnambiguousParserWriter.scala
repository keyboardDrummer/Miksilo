package core.parsers.core

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds

trait UnambiguousParserWriter extends ParserWriter {
  type ParseResult[+Result] <: UnambiguousParseResult[Result]

  trait UnambiguousParseResult[+Result] extends ParseResultLike[Result] {
    def getSuccessRemainder: Option[Input]
    override def successful: Boolean = getSuccessRemainder.nonEmpty
    def get: Result
  }

  class CheckCache[Result](parseState: PackratParseState, parser: Parser[Result])
    extends ParserState[Result](parseState, parser)
    with Parse[Result] {

    val cache = mutable.HashMap[Input, ParseResult[Result]]()

    def apply(input: Input) = {
      cache.get (input) match {
        case None =>
          parseState.callStack.push(parser)
          val value: ParseResult[Result] = parser.parseInternal(input)
          parseState.callStack.pop()
          if (!isPartOfACycle) {
            cache.put (input, value)
          }
          value
        case Some (result) => result
      }
    }
  }

  trait FixPoint[Result] {
    def parseState: PackratParseState
    def parser: Parser[Result]

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
    final def growResult(input: Input, previous: ParseResult[Result]): ParseResult[Result] = {
      recursionIntermediates.put(input, previous)

      val nextResult: ParseResult[Result] = parser.parseInternal(input)
      nextResult.getSuccessRemainder match {
        case Some(remainder) if remainder.offset > previous.getSuccessRemainder.get.offset =>
          growResult(input, nextResult)
        case _ =>
          recursionIntermediates.remove(input)
          previous
      }
    }
  }

  class DoFixPoint[Result](parseState: PackratParseState, parser: Parser[Result])
    extends ParserState[Result](parseState, parser) with FixPoint[Result] with Parse[Result] {

    override def apply(input: Input) = {
      getPreviousResult(input) match {
        case None =>

          callStackSet.add(input)
          parseState.callStack.push(parser)
          var result = parser.parseInternal(input)
          if (result.successful && hasBackEdge) {
            result = growResult(input, result)
          }
          callStackSet.remove(input)
          parseState.callStack.pop()
          result

        case Some(result) => result
      }
    }
  }

  class FixPointAndCache[Result](parseState: PackratParseState, parser: Parser[Result])
    extends CheckCache(parseState, parser) with FixPoint[Result] {

    override def apply(input: Input) = {
      cache.get(input) match {
        case None =>

          val value = getPreviousResult(input) match {
            case None =>

              callStackSet.add(input)
              parseState.callStack.push(parser)
              var result = parser.parseInternal(input)
              if (result.successful && hasBackEdge) {
                result = growResult(input, result)
              }
              callStackSet.remove(input)
              parseState.callStack.pop()
              result

            case Some(result) => result
          }

          if (!isPartOfACycle) {
            cache.put(input, value)
          }
          value
        case Some(result) => result
      }
    }
  }

  class ParserState[Result](val parseState: PackratParseState, val parser: Parser[Result]) {

    var isPartOfACycle: Boolean = false
  }

  override def getParse[Result](parseState: ParseState,
                                parser: ParserBase[Result],
                                shouldCache: Boolean,
                                shouldDetectLeftRecursion: Boolean): Parse[Result] = {
    if (!shouldCache && !shouldDetectLeftRecursion) {
      return parser.parseInternal
    }
    if (shouldCache && shouldDetectLeftRecursion) {
      return parseState.parserStates.getOrElseUpdate(parser, new FixPointAndCache[Any](parseState, parser)).asInstanceOf[Parse[Result]]
    }

    if (shouldCache) {
      return parseState.parserStates.getOrElseUpdate(parser, new CheckCache[Any](parseState, parser)).asInstanceOf[Parse[Result]]
    }

    parseState.parserStates.getOrElseUpdate(parser, new DoFixPoint[Any](parseState, parser)).asInstanceOf[Parse[Result]]
  }

  type ParseState = PackratParseState
  class PackratParseState {
    val parserStates = mutable.HashMap[Parser[Any], ParserState[Any]]()
    val callStack = mutable.Stack[Parser[Any]]()
  }
}
