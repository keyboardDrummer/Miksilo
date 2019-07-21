package core.parsers.core

import scala.collection.mutable
import scala.language.higherKinds

trait OptimizingParserWriter extends ParserWriter {

  type Self[+Result] = ParserBuilder[Result]

  def wrapParser[Result](parser: Parser[Result],
                         shouldCache: Boolean, shouldDetectLeftRecursion: Boolean): Parser[Result]

  def newParseState(input: Input): ParseState
  type ParseState

  trait Parser[+Result] {
    def apply(input: Input, state: ParseState): ParseResult[Result]
    def debugName: Any = null
  }

  trait GetParser {
    def apply[Result](parser: Self[Result]): Parser[Result]
  }

  trait ParserBuilder[+Result] {
    def getParser(recursive: GetParser): Parser[Result]
    def mustConsumeInput: Boolean
    def getMustConsume(cache: ConsumeCache): Boolean
    def leftChildren: List[ParserBuilder[_]]
    def children: List[ParserBuilder[_]]
  }

  trait ParserBuilderBase[Result] extends ParserBuilder[Result] {
    var mustConsumeInput: Boolean = false
  }

  trait SequenceLike[+Result] extends ParserBuilder[Result] {
    def left: Self[Any]
    def right: Self[Any]

    override def children = List(left, right)

    override def leftChildren: List[ParserBuilder[Any]] = if (left.mustConsumeInput) List(left) else List(left, right)

    override def getMustConsume(cache: ConsumeCache) = cache(left) || cache(right)
  }

  trait ChoiceLike[+Result] extends ParserBuilder[Result] {
    def first: Self[Result]
    def second: Self[Result]

    override def children = List(first, second)

    override def leftChildren = List(first, second)

    override def getMustConsume(cache: ConsumeCache) = cache(first) && cache(second)
  }

  trait ParserWrapper[+Result] extends ParserBuilder[Result] {
    def original: ParserBuilder[Any]

    override def getMustConsume(cache: ConsumeCache) = cache(original)

    override def leftChildren = List(original)

    override def children = List(original)
  }

  class Lazy[Result](_original: => Self[Result], val debugName: Any = null) extends ParserBuilderBase[Result] with ParserWrapper[Result] {
    lazy val original: Self[Result] = _original
    def getOriginal = original

    override def getParser(recursive: GetParser): Parser[Result] = {
      lazy val parseOriginal = recursive(original)
      new Parser[Result] {
        override def apply(input: Input, state: ParseState) = {
          parseOriginal(input, state)
        }

        override def debugName = Lazy.this.debugName

        override def toString = if (debugName != null) debugName.toString else super.toString
      }
    }

    override def toString = if (debugName != null) debugName.toString else super.toString

    override def leftChildren = List(original)

    override def getMustConsume(cache: ConsumeCache) = cache(original)
  }

  trait LeafParser[+Result] extends ParserBuilder[Result] {
    override def leftChildren = List.empty

    override def children = List.empty
  }

  case class MapParser[Result, NewResult](original: Self[Result], f: Result => NewResult)
    extends ParserBuilderBase[NewResult] with ParserWrapper[NewResult] {

    override def getParser(recursive: GetParser): Parser[NewResult] = {
      val parseOriginal = recursive(original)
      (input, state) => parseOriginal(input, state).map(f)
    }
  }

  def compile[Result](root: Self[Result]): ParserAnalysis = {
    var nodesThatShouldDetectLeftRecursion: Set[ParserBuilder[_]] = Set.empty
    val mustConsumeCache = new ConsumeCache

    val reverseGraph = mutable.HashMap.empty[ParserBuilder[_], mutable.Set[ParserBuilder[_]]]
    GraphAlgorithms.depthFirst[ParserBuilder[_]](root,
      node => {
        node.asInstanceOf[ParserBuilderBase[Any]].mustConsumeInput = mustConsumeCache(node)
        node.children
      },
      (_, path: List[ParserBuilder[_]]) => path match {
        case child :: parent :: _ =>
          val incoming = reverseGraph.getOrElseUpdate(child, mutable.HashSet.empty)
          incoming.add(parent)
        case _ =>
      },
      cycle => {
          nodesThatShouldDetectLeftRecursion += cycle.head
      })

    val components = SCC.scc[ParserBuilder[_]](reverseGraph.keys.toSet, node => node.leftChildren.toSet)
    val nodesInCycle: Set[ParserBuilder[_]] = components.filter(c => c.size > 1).flatten.toSet

    val nodesWithMultipleIncomingEdges: Set[ParserBuilder[_]] = reverseGraph.filter(e => e._2.size > 1).keys.toSet
    val nodesWithIncomingCycleEdge: Set[ParserBuilder[_]] = reverseGraph.filter(e => e._2.exists(parent => nodesInCycle.contains(parent))).keys.toSet
    val nodesThatShouldCache: Set[ParserBuilder[_]] = nodesWithIncomingCycleEdge ++ nodesWithMultipleIncomingEdges

    ParserAnalysis(nodesThatShouldCache, nodesThatShouldDetectLeftRecursion)
  }

  case class ParserAnalysis(nodesThatShouldCache: Set[ParserBuilder[_]], nodesThatShouldDetectLeftRecursion: Set[ParserBuilder[_]]) {

    def buildParser[Result](root: Self[Result]): Parser[Result] = {
      val cacheOfParses = new mutable.HashMap[Self[Any], Parser[Any]]
      var caches = List.empty[CheckCache[_]]
      def recursive: GetParser = new GetParser {
        override def apply[SomeResult](_parser: Self[SomeResult]): Parser[SomeResult] = {
          cacheOfParses.getOrElseUpdate(_parser, {
            val parser = _parser.asInstanceOf[ParserBuilder[SomeResult]]
            val result = parser.getParser(recursive)
            val wrappedParser = wrapParser(result, nodesThatShouldCache(parser), nodesThatShouldDetectLeftRecursion(parser))
            wrappedParser match {
              case check: CheckCache[_] => caches ::= check
              case _ =>
            }
            wrappedParser
          }).asInstanceOf[Parser[SomeResult]]
        }
      }

      val wrappedRoot = recursive(root)
      val reusableParser: Parser[Result] = (input, state) => {
        caches.foreach(cache => cache.cache.clear())
        wrappedRoot.apply(input, state)
      }
      reusableParser
    }
  }

  class ConsumeCache {
    var values = mutable.Map.empty[ParserBuilder[Any], Boolean]

    def apply[Result](parser: ParserBuilder[Any]): Boolean = {
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

    def addAlternative[Other >: Result](getAlternative: (Self[Other], Self[Other]) => Self[Other], debugName: Any = null): Self[Other] = {
      lazy val result: Self[Other] = new Lazy(parser | getAlternative(parser, result), debugName)
      result
    }
  }

  class CheckCache[Result](parser: Parser[Result]) extends Parser[Result] {
    // TODO I can differentiate between recursive and non-recursive results. Only the former depend on the state.
    val cache = mutable.HashMap[(Input, ParseState), ParseResult[Result]]()

    def apply(input: Input, state: ParseState): ParseResult[Result] = {
      val key = (input, state)
      cache.get(key) match {
        case Some(value) =>
          value
        case _ =>
          val value: ParseResult[Result] = parser(input, state)
          cache.put(key, value)
          value
      }
    }
  }
}
