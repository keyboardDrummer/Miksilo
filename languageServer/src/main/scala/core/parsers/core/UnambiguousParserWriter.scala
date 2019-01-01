package core.parsers.core

import util.cache.Cache

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

  class PackratParseState(val resultCache: Cache[ParseNode, ParseResult[Any]], val extraState: ExtraState) extends ParseStateLike {

    val recursionIntermediates = mutable.HashMap[ParseNode, ParseResult[Any]]()
    val callStackSet = mutable.HashSet[ParseNode]()
    val callStack = mutable.Stack[Parser[Any]]()
    var parsersPartOfACycle: Set[Parser[Any]] = Set.empty
    val parsersWithBackEdges = mutable.HashSet[Parser[Any]]()

    def parse[Result](parser: Parser[Result], input: Input): ParseResult[Result] = {

      val node = ParseNode(input, parser)
      resultCache.get(node).getOrElse({
        val value: ParseResult[Result] = parseIteratively[Result](parser, input)
        if (!parsersPartOfACycle.contains(parser)) {
          resultCache.add(node, value)
        }
        value
      }).asInstanceOf[ParseResult[Result]]
    }

    def parseIteratively[Result](parser: Parser[Result], input: Input): ParseResult[Result] = {
      val node = ParseNode(input, parser)
      getPreviousResult(node) match {
        case None =>

          callStackSet.add(node)
          callStack.push(node.parser)
          var result = parser.parseInternal(input, this)
          if (result.successful && parsersWithBackEdges.contains(parser)) {
            result = growResult(node, parser, result, this)
          }
          callStackSet.remove(node)
          callStack.pop()
          result

        case Some(result) => result
      }
    }

    @tailrec
    private def growResult[Result](node: ParseNode, parser: Parser[Result], previous: ParseResult[Result], state: ParseStateLike): ParseResult[Result] = {
      recursionIntermediates.put(node, previous)

      val nextResult: ParseResult[Result] = parser.parseInternal(node.input, state)
      nextResult.getSuccessRemainder match {
        case Some(remainder) if remainder.offset > previous.getSuccessRemainder.get.offset =>
          growResult(node, parser, nextResult, state)
        case _ =>
          recursionIntermediates.remove(node)
          previous
      }
    }

    def getPreviousResult[Result](node: ParseNode): Option[ParseResult[Result]] = {
      if (callStackSet.contains(node)) {
        parsersWithBackEdges.add(node.parser)
        val index = callStack.indexOf(node.parser)
        parsersPartOfACycle ++= callStack.take(index + 1)
        return Some(recursionIntermediates.getOrElse(node, abort).asInstanceOf[ParseResult[Result]])
      }
      None
    }
  }
}
