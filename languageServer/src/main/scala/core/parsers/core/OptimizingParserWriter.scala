package core.parsers.core

import scala.collection.mutable
import scala.language.higherKinds

trait OptimizingParserWriter extends ParserWriter {

  type Self[+Result] = LRParser[Result]

  def wrapParse[Result](parser: Parse[Result],
                        shouldCache: Boolean, shouldDetectLeftRecursion: Boolean): Parse[Result]


  def newParseState(input: Input): ParseState
  type ParseState

  trait Parse[+Result] {
    def apply(input: Input, state: ParseState): ParseResult[Result]
  }

  trait GetParse {
    def apply[Result](parser: Parser[Result]): Parse[Result]
  }

  trait Parser[+Result] {
    def getParser(recursive: GetParse): Parse[Result]
  }

  trait LRParser[+Result] extends super.Parser[Result] {
    def mustConsumeInput: Boolean
    def getMustConsume(cache: ConsumeCache): Boolean
    def leftChildren: List[LRParser[_]]
    def children: List[LRParser[_]]
  }

  trait ParserBase[Result] extends LRParser[Result] {
    var mustConsumeInput: Boolean = false
  }

  trait SequenceLike[+Result] extends LRParser[Result] {
    def left: Self[Any]
    def right: Self[Any]

    override def children = List(left, right)

    override def leftChildren = if (left.mustConsumeInput) List(left) else List(left, right)

    override def getMustConsume(cache: ConsumeCache) = cache(left) || cache(right)
  }

  trait ChoiceLike[+Result] extends LRParser[Result] {
    def first: Self[Result]
    def second: Self[Result]

    override def children = List(first, second)

    override def leftChildren = List(first, second)

    override def getMustConsume(cache: ConsumeCache) = cache(first) && cache(second)
  }

  trait ParserWrapper[+Result] extends LRParser[Result] {
    def original: LRParser[Any]

    override def getMustConsume(cache: ConsumeCache) = cache(original)

    override def leftChildren = List(original)

    override def children = List(original)
  }

  class Lazy[Result](_original: => Self[Result]) extends ParserBase[Result] with ParserWrapper[Result] {
    lazy val original: Self[Result] = _original
    def getOriginal = original

    override def getParser(recursive: GetParse): Parse[Result] = {
      lazy val parseOriginal = recursive(original)
      (input, state) => parseOriginal(input, state)
    }

    override def leftChildren = List(original)

    override def getMustConsume(cache: ConsumeCache) = cache(original)
  }

  trait LeafParser[+Result] extends LRParser[Result] {
    override def leftChildren = List.empty

    override def children = List.empty
  }

  case class MapParser[Result, NewResult](original: Self[Result], f: Result => NewResult)
    extends ParserBase[NewResult] with ParserWrapper[NewResult] {

    override def getParser(recursive: GetParse): Parse[NewResult] = {
      val parseOriginal = recursive(original)
      (input, state) => parseOriginal(input, state).map(f)
    }
  }

  def compile[Result](root: Self[Result]): ParserAnalysis = {
    var nodesThatShouldDetectLeftRecursion: Set[LRParser[_]] = Set.empty
    val mustConsumeCache = new ConsumeCache

    val reverseGraph = mutable.HashMap.empty[LRParser[_], mutable.Set[LRParser[_]]]
    GraphAlgorithms.depthFirst[LRParser[_]](root,
      node => {
        node.asInstanceOf[ParserBase[Any]].mustConsumeInput = mustConsumeCache(node)
        node.children
      },
      (_, path: List[LRParser[_]]) => path match {
        case child :: parent :: _ =>
          val incoming = reverseGraph.getOrElseUpdate(child, mutable.HashSet.empty)
          incoming.add(parent)
        case _ =>
      },
      cycle => {
        nodesThatShouldDetectLeftRecursion += cycle.head
      })

    val components = SCC.scc[LRParser[_]](reverseGraph.keys.toSet, node => node.leftChildren.toSet)
    val nodesInCycle: Set[LRParser[_]] = components.filter(c => c.size > 1).flatten.toSet

    val nodesWithMultipleIncomingEdges: Set[LRParser[_]] = reverseGraph.filter(e => e._2.size > 1).keys.toSet
    val nodesWithIncomingCycleEdge: Set[LRParser[_]] = reverseGraph.filter(e => e._2.exists(parent => nodesInCycle.contains(parent))).keys.toSet
    val nodesThatShouldCache: Set[LRParser[_]] = nodesWithIncomingCycleEdge ++ nodesWithMultipleIncomingEdges

    ParserAnalysis(nodesThatShouldCache, nodesThatShouldDetectLeftRecursion)
  }

  case class ParserAnalysis(nodesThatShouldCache: Set[LRParser[_]], nodesThatShouldDetectLeftRecursion: Set[LRParser[_]]) {

    def getParse[Result](root: Self[Result]): Parse[Result] = {
      var cacheOfParses = new mutable.HashMap[Parser[Any], Parse[Any]]

      def recursive: GetParse = new GetParse {
        override def apply[SomeResult](_parser: Parser[SomeResult]): Parse[SomeResult] = {
          cacheOfParses.getOrElseUpdate(_parser, {
            val parser = _parser.asInstanceOf[LRParser[SomeResult]]
            val result = parser.getParser(recursive)
            wrapParse(result, nodesThatShouldCache(parser), nodesThatShouldDetectLeftRecursion(parser))
          }).asInstanceOf[Parse[SomeResult]]
        }
      }

      recursive(root)
    }
  }

  class ConsumeCache {
    var values = mutable.Map.empty[LRParser[Any], Boolean]

    def apply[Result](parser: LRParser[Any]): Boolean = {
      values.get(parser) match {
        case Some(v) => v
        case None =>
          values.put(parser, false)
          val value = parser.getMustConsume(this)
          values.put(parser, value)
          value
      }
    }
  }

  implicit class ParserExtensions[+Result](parser: Self[Result]) extends super.ParserExtensions(parser) {

    def addAlternative[Other >: Result](getAlternative: (Self[Other], Self[Other]) => Self[Other]): Self[Other] = {
      lazy val result: Self[Other] = new Lazy(parser | getAlternative(parser, result))
      result
    }

    def parseRoot(input: Input): ParseResult[Result] = {
      val analysis = compile(parser)
      analysis.getParse(parser)(input, newParseState(input))
    }
  }
}
