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

  class ParserState[Result](val parser: Parser[Result]) {
    val recursionIntermediates = mutable.HashMap[Input, ParseResult[Result]]()
    val callStackSet = mutable.HashSet[Input]() // TODO might not be needed if we put an abort in the intermediates.
    var isPartOfACycle: Boolean = false
    var hasBackEdge: Boolean = false
    val cache = mutable.HashMap[Input, ParseResult[Result]]()
  }

  class PackratParseState(val extraState: ExtraState) extends ParseStateLike {

    val parserStates = mutable.HashMap[Parser[Any], ParserState[Any]]()
    val callStack = mutable.Stack[Parser[Any]]()

    def parse[Result](parser: Parser[Result], input: Input): ParseResult[Result] = {

      val parserState = parserStates.getOrElseUpdate(parser, new ParserState(parser)).asInstanceOf[ParserState[Result]]
      parserState.cache.getOrElse(input, {
        val value: ParseResult[Result] = parseIteratively[Result](parserState, input)
        if (!parserState.isPartOfACycle) {
          parserState.cache.put(input, value)
        }
        value
      })
    }

    def parseIteratively[Result](parserState: ParserState[Result], input: Input): ParseResult[Result] = {
      getPreviousResult(parserState, input) match {
        case None =>

          parserState.callStackSet.add(input)
          callStack.push(parserState.parser)
          var result = parserState.parser.parseInternal(input, this)
          if (result.successful && parserState.hasBackEdge) {
            result = growResult(parserState, input, result, this)
          }
          parserState.callStackSet.remove(input)
          callStack.pop()
          result

        case Some(result) => result
      }
    }

    @tailrec
    private def growResult[Result](parserState: ParserState[Result], input: Input, previous: ParseResult[Result], state: ParseStateLike): ParseResult[Result] = {
      parserState.recursionIntermediates.put(input, previous)

      val nextResult: ParseResult[Result] = parserState.parser.parseInternal(input, state)
      nextResult.getSuccessRemainder match {
        case Some(remainder) if remainder.offset > previous.getSuccessRemainder.get.offset =>
          growResult(parserState, input, nextResult, state)
        case _ =>
          parserState.recursionIntermediates.remove(input)
          previous
      }
    }

    def getPreviousResult[Result](parserState: ParserState[Result], input: Input): Option[ParseResult[Result]] = {
      if (parserState.callStackSet.contains(input)) {
        if (!parserState.recursionIntermediates.contains(input)) {
          parserState.hasBackEdge = true
          val index = callStack.indexOf(parserState.parser)
          callStack.take(index + 1).foreach(parser => parserStates(parser).isPartOfACycle = true) // TODO this would also be possible by returning a value that indicates we found a cycle, like the abort!
        }
        return Some(parserState.recursionIntermediates.getOrElse(input, abort))
      }
      None
    }
  }
}
