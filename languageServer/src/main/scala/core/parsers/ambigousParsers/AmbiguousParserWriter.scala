package core.parsers.ambigousParsers

import core.parsers.core.LeftRecursiveParserWriter
import util.cache.{Cache, InfiniteCache}

import scala.collection.mutable
import scala.language.higherKinds

trait AmbiguousParserWriter extends LeftRecursiveParserWriter {

  case class ParseNode(input: Input, parser: Parse[Any])

  type ParseResult[+Result] <: AmbiguousParseResult[Result]
  type ParseState = LeftRecursionDetectorState

  override def wrapParse[Result](parseState: LeftRecursionDetectorState,
                                 parser: Parse[Result],
                                 shouldCache: Boolean,
                                 shouldDetectLeftRecursion: Boolean) =
    (input: Input) => parseState.parse(parser, input)

  trait AmbiguousParseResult[+Result] extends ParseResultLike[Result] {
    def getSingleSuccesses: List[SingleSuccess[Result]]
  }

  def combineSuccesses[Result](successes: Seq[ParseResult[Result]]): ParseResult[Result]

  case class SingleSuccess[+Result](result: ParseResult[Result], remainder: Input)

  class LeftRecursionDetectorState {

    val resultCache: Cache[ParseNode, ParseResult[Any]] = new InfiniteCache[ParseNode, ParseResult[Any]]()
    val recursionIntermediates = mutable.HashMap[ParseNode, ParseResult[Any]]()
    val callStackSet = mutable.HashSet[ParseNode]()
    val callStack = mutable.Stack[Parse[Any]]()
    var parsersPartOfACycle: Set[Parse[Any]] = Set.empty
    val parsersWithBackEdges = mutable.HashSet[Parse[Any]]()

    def parse[Result](parser: Parse[Result], input: Input): ParseResult[Result] = {

      val node = ParseNode(input, parser)
      resultCache.get(node).getOrElse({
        val value: ParseResult[Result] = parseIteratively[Result](parser, input)
        if (!parsersPartOfACycle.contains(parser)) {
          resultCache.add(node, value)
        }
        value
      }).asInstanceOf[ParseResult[Result]]
    }

    def parseIteratively[Result](parser: Parse[Result], input: Input): ParseResult[Result] = {
      val node = ParseNode(input, parser)
      getPreviousResult(node) match {
        case None =>

          callStackSet.add(node)
          callStack.push(node.parser)
          var result = parser(input)
          if (result.successful && parsersWithBackEdges.contains(parser)) {
            result = growResult(node, parser, result, this)
          }
          callStackSet.remove(node)
          callStack.pop()
          result

        case Some(result) => result
      }
    }

    private def growResult[Result](node: ParseNode, parser: Parse[Result], previous: ParseResult[Result], state: ParseState): ParseResult[Result] = {
      var intermediatesToGrow: List[SingleSuccess[Result]] = previous.getSingleSuccesses
      var endResults: List[ParseResult[Result]] = List.empty
      var visited = mutable.Set.empty[ParseResult[Result]]
      while(intermediatesToGrow.nonEmpty) {
        val intermediate = intermediatesToGrow.head
        intermediatesToGrow = intermediatesToGrow.tail
        if (visited.add(intermediate.result)) {
          /** The traversed back edge failure krijgt een partial result, hoe kan dat?
            * Komt door de addDefault in de Choice, die er volgens mij in zit om many goed te laten defaulten.
            * De recursive failure met partial result wordt elke cycle eentje langer gemaakt, en zo loopt de visited stuk.
            * Eigenlijk wil ik nooit een partial result uit die cycle detection failure halen, misschien kan ik een speciaal soort failure gebruiken?
            */

          recursionIntermediates.put(node, intermediate.result)
          val nextResult: ParseResult[Result] = parser(node.input)
          val singletons = nextResult.getSingleSuccesses
          val grew = singletons.exists(s => s.remainder.offset > intermediate.remainder.offset)
          if (grew) {
            intermediatesToGrow ++= singletons.filter(s => s.remainder.offset > intermediate.remainder.offset)
          } else {
            endResults ::= intermediate.result
          }
        }
      }
      recursionIntermediates.remove(node)
      combineSuccesses(endResults.reverse)
    }

    def getPreviousResult[Result](node: ParseNode): Option[ParseResult[Result]] = {
      if (callStackSet.contains(node)) {
        parsersWithBackEdges.add(node.parser)
        val index = callStack.indexOf(node.parser)
        parsersPartOfACycle ++= callStack.take(index + 1)
        return Some(recursionIntermediates.getOrElse(node, abort).
          asInstanceOf[ParseResult[Result]])
      }
      None
    }
  }
}
