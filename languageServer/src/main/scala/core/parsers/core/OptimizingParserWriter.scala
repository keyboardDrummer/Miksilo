package core.parsers.core

import scala.collection.mutable
import scala.language.higherKinds

trait OptimizingParserWriter extends ParserWriter {

  type Self[+Result] = OptimizingParser[Result]

  def wrapParse[Result](parser: Parse[Result],
                        shouldCache: Boolean, shouldDetectLeftRecursion: Boolean): Parse[Result]

  def newParseState(input: Input): ParseState
  type ParseState

  trait Parse[+Result] {
    def apply(input: Input, state: ParseState): ParseResult[Result]
    def debugName: Any = null
  }

  trait GetParse {
    def apply[Result](parser: Self[Result]): Parse[Result]
  }

  trait OptimizingParser[+Result] {
    def getParser(recursive: GetParse): Parse[Result]
    def mustConsumeInput: Boolean
    def getMustConsume(cache: ConsumeCache): Boolean
    def leftChildren: List[OptimizingParser[_]]
    def children: List[OptimizingParser[_]]
  }

  trait ParserBase[Result] extends OptimizingParser[Result] {
    var mustConsumeInput: Boolean = false
  }

  trait SequenceLike[+Result] extends OptimizingParser[Result] {
    def left: Self[Any]
    def right: Self[Any]

    override def children = List(left, right)

    override def leftChildren: List[OptimizingParser[Any]] = if (left.mustConsumeInput) List(left) else List(left, right)

    override def getMustConsume(cache: ConsumeCache) = cache(left) || cache(right)
  }

  trait ChoiceLike[+Result] extends OptimizingParser[Result] {
    def first: Self[Result]
    def second: Self[Result]

    override def children = List(first, second)

    override def leftChildren = List(first, second)

    override def getMustConsume(cache: ConsumeCache) = cache(first) && cache(second)
  }

  trait ParserWrapper[+Result] extends OptimizingParser[Result] {
    def original: OptimizingParser[Any]

    override def getMustConsume(cache: ConsumeCache) = cache(original)

    override def leftChildren = List(original)

    override def children = List(original)
  }

  class Lazy[Result](_original: => Self[Result], val debugName: Any = null) extends ParserBase[Result] with ParserWrapper[Result] {
    lazy val original: Self[Result] = _original
    def getOriginal = original

    override def getParser(recursive: GetParse): Parse[Result] = {
      lazy val parseOriginal = recursive(original)
      new Parse[Result] {
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

  trait LeafParser[+Result] extends OptimizingParser[Result] {
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
    var nodesThatShouldDetectLeftRecursion: Set[OptimizingParser[_]] = Set.empty
    val mustConsumeCache = new ConsumeCache

    val reverseGraph = mutable.HashMap.empty[OptimizingParser[_], mutable.Set[OptimizingParser[_]]]
    GraphAlgorithms.depthFirst[OptimizingParser[_]](root,
      node => {
        node.asInstanceOf[ParserBase[Any]].mustConsumeInput = mustConsumeCache(node)
        node.children
      },
      (_, path: List[OptimizingParser[_]]) => path match {
        case child :: parent :: _ =>
          val incoming = reverseGraph.getOrElseUpdate(child, mutable.HashSet.empty)
          incoming.add(parent)
        case _ =>
      },
      cycle => {
        val cycleArray = cycle.toArray
        val leftRecursive = cycleArray.indices.forall(index => {
          val left = cycleArray(index)
          val right = cycleArray((index + 1) % cycleArray.length)
          right.leftChildren.contains(left)
        })
        if (leftRecursive)
          nodesThatShouldDetectLeftRecursion += cycle.head
      })

    val components = SCC.scc[OptimizingParser[_]](reverseGraph.keys.toSet, node => node.leftChildren.toSet)
    val nodesInCycle: Set[OptimizingParser[_]] = components.filter(c => c.size > 1).flatten.toSet

    val nodesWithMultipleIncomingEdges: Set[OptimizingParser[_]] = reverseGraph.filter(e => e._2.size > 1).keys.toSet
    val nodesWithIncomingCycleEdge: Set[OptimizingParser[_]] = reverseGraph.filter(e => e._2.exists(parent => nodesInCycle.contains(parent))).keys.toSet
    val nodesThatShouldCache: Set[OptimizingParser[_]] = nodesWithIncomingCycleEdge ++ nodesWithMultipleIncomingEdges

    ParserAnalysis(nodesThatShouldCache, nodesThatShouldDetectLeftRecursion)
  }

  case class ParserAnalysis(nodesThatShouldCache: Set[OptimizingParser[_]], nodesThatShouldDetectLeftRecursion: Set[OptimizingParser[_]]) {

    def getParse[Result](root: Self[Result]): Parse[Result] = {
      var cacheOfParses = new mutable.HashMap[Self[Any], Parse[Any]]

      def recursive: GetParse = new GetParse {
        override def apply[SomeResult](_parser: Self[SomeResult]): Parse[SomeResult] = {
          cacheOfParses.getOrElseUpdate(_parser, {
            val parser = _parser.asInstanceOf[OptimizingParser[SomeResult]]
            val result = parser.getParser(recursive)
            wrapParse(result, nodesThatShouldCache(parser), nodesThatShouldDetectLeftRecursion(parser))
          }).asInstanceOf[Parse[SomeResult]]
        }
      }

      recursive(root)
    }
  }

  class ConsumeCache {
    var values = mutable.Map.empty[OptimizingParser[Any], Boolean]

    def apply[Result](parser: OptimizingParser[Any]): Boolean = {
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

    def parseRoot(input: Input): ParseResult[Result] = {
      val analysis = compile(parser) // TODO don't compile all the time.
      analysis.getParse(parser)(input, newParseState(input))
    }
  }
}
