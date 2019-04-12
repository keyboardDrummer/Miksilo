package core.parsers.core

import util.ExtendedType

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.language.higherKinds

trait LeftRecursiveParserWriter extends ParserWriter {

  type Self[+Result] <: LRParser[Result]

  def lazyParser[Result](inner: => Self[Result]): Self[Result]

  def newParseState(root: Self[_]): ParseState

  def wrapParse[Result](parseState: ParseState,
                        parser: Parse[Result],
                        shouldCache: Boolean, shouldDetectLeftRecursion: Boolean): Parse[Result]


  trait LRParser[+Result] extends super.Parser[Result] {
    def mustConsume: Boolean
    def getMustConsume(cache: ConsumeCache): Boolean
    def leftChildren: List[LRParser[_]]
    def children: List[LRParser[_]]
  }

  trait ParserBase[Result] extends LRParser[Result] {
    var staticCycle: Boolean = false
    var mustConsume: Boolean = false
  }

  trait SequenceLike[+Result] extends LRParser[Result] {
    def left: Self[Any]
    def right: Self[Any]

    override def children = List(left, right)

    override def leftChildren = if (left.mustConsume) List(left) else List(left, right)

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

    override def getParser(recursive: HasRecursive): Parse[Result] = {
      lazy val r = recursive(original)
      input => r(input)
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

    override def getParser(recursive: HasRecursive): Parse[NewResult] = {
      val parseOriginal = recursive(original)
      input => parseOriginal(input).map(f)
    }

    override def leftChildren = List(original)

    override def getMustConsume(cache: ConsumeCache) = cache(original)
  }

  def compile[Result](root: Self[Result]): ParserAnalysis = {
    var nodesThatShouldDetectLeftRecursion: Set[LRParser[_]] = Set.empty
    val mustConsumeCache = new ConsumeCache

    val reverseGraph = mutable.HashMap.empty[LRParser[_], mutable.Set[LRParser[_]]]
    GraphAlgorithms.depthFirst[LRParser[_]](root,
      node => {
        node.asInstanceOf[ParserBase[Any]].mustConsume = mustConsumeCache(node)
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

    GraphAlgorithms.depthFirst[LRParser[_]](root,
      node => {
        node.leftChildren
      },
      (_, path: List[LRParser[_]]) => {},
      cycle => {
        nodesThatShouldDetectLeftRecursion += cycle.head
      })

    val components = SCC.scc[LRParser[_]](reverseGraph.keys.toSet, node => node.leftChildren.toSet)
    val nodesInCycle: Set[LRParser[_]] = components.filter(c => c.size > 1).flatten.toSet

    val nodesWithMultipleIncomingEdges: Set[LRParser[_]] = reverseGraph.filter(e => e._2.size > 1).keys.toSet
    val nodesWithIncomingCycleEdge: Set[LRParser[_]] = reverseGraph.filter(e => e._2.exists(parent => nodesInCycle.contains(parent))).keys.toSet
    val nodesThatShouldCache: Set[LRParser[_]] = nodesWithIncomingCycleEdge ++ nodesWithMultipleIncomingEdges

    nodesInCycle.foreach(n => n.asInstanceOf[ParserBase[Any]].staticCycle = true)
    ParserAnalysis(nodesThatShouldCache, nodesThatShouldDetectLeftRecursion)
  }

  case class ParserAnalysis(nodesThatShouldCache: Set[LRParser[_]], nodesThatShouldDetectLeftRecursion: Set[LRParser[_]]) {

    def getParse[Result](root: Self[Result]): Parse[Result] = {
      val parseState = newParseState(root)

      def recursive: HasRecursive = new HasRecursive {
        override def apply[SomeResult](_parser: Parser[SomeResult]): Parse[SomeResult] = {
          val parser = _parser.asInstanceOf[LRParser[SomeResult]]
          val result = parser.getParser(recursive)
          wrapParse(parseState, result, nodesThatShouldCache(parser), nodesThatShouldDetectLeftRecursion(parser))
        }
      }

      recursive(root)
    }
  }

  val cache: TrieMap[Class[_], List[LRParser[_] => LRParser[_]]] = TrieMap.empty

  def getChildProperties(clazz: Class[LRParser[_]]): List[LRParser[_] => LRParser[_]] = {
    cache.getOrElseUpdate(clazz, new ExtendedType[LRParser[_]](clazz).fieldsOfType[LRParser[_]](classOf[LRParser[_]]))
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
      lazy val result: Self[Other] = lazyParser(parser | getAlternative(parser, result))
      result
    }

    def parseRoot(input: Input): ParseResult[Result] = {
      val analysis = compile(parser)
      analysis.getParse(parser)(input)
    }
  }
}
