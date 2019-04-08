package core.parsers.core

import util.ExtendedType

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.language.higherKinds

trait LeftRecursiveParserWriter extends ParserWriter {

  type Self[+Result] <: Parser2[Result]

  def lazyParser[Result](inner: => Self[Result]): Self[Result]

  def newParseState(root: Self[_]): ParseState

  def getParse[Result](parseState: ParseState,
                       parser: ParserBase[Result],
                       shouldCache: Boolean, shouldDetectLeftRecursion: Boolean): Parse[Result]

  trait Parse[+Result] {
    def apply(input: Input): ParseResult[Result]
  }

  trait Parser2[+Result] extends Parser[Result] with Parse[Result] {
    def mustConsume: Boolean
    def getMustConsume(cache: ConsumeCache): Boolean
    def leftChildren: List[Parser2[_]]
    def children: List[Parser2[_]]
    def parse: Parse[Result]
  }

  trait ParserBase[Result] extends Parser2[Result] {
    var parse: Parse[Result] = this
    var staticCycle: Boolean = false
    var mustConsume: Boolean = false
  }

  trait SequenceLike[+Result] extends Parser2[Result] {
    def left: Self[Any]
    def right: Self[Any]

    override def children = List(left, right)

    override def leftChildren = if (left.mustConsume) List(left) else List(left, right)

    override def getMustConsume(cache: ConsumeCache) = cache(left) || cache(right)
  }

  trait ChoiceLike[+Result] extends Parser2[Result] {
    def first: Self[Result]
    def second: Self[Result]

    override def children = List(first, second)

    override def leftChildren = List(first, second)

    override def getMustConsume(cache: ConsumeCache) = cache(first) && cache(second)
  }

  trait ParserWrapper[+Result] extends Parser2[Result] {
    def original: Parser2[Any]

    override def getMustConsume(cache: ConsumeCache) = cache(original)

    override def leftChildren = List(original)

    override def children = List(original)
  }

  class Lazy[Result](_original: => Self[Result]) extends ParserBase[Result] with ParserWrapper[Result] {
    lazy val original: Self[Result] = _original

    override def apply(input: Input) = original.parse(input)

    override def leftChildren = List(original)

    override def getMustConsume(cache: ConsumeCache) = cache(original)
  }

  trait LeafParser[+Result] extends Parser2[Result] {
    override def leftChildren = List.empty

    override def children = List.empty
  }

  case class MapParser[Result, NewResult](original: Self[Result], f: Result => NewResult)
    extends ParserBase[NewResult] with ParserWrapper[NewResult] {

    override def apply(input: Input) = {
      val result = original.parse(input)
      result.map(f)
    }

    override def leftChildren = List(original)

    override def getMustConsume(cache: ConsumeCache) = cache(original)
  }

  def compile[Result](root: Self[Result]): ParseState = {
    var nodesThatShouldDetectLeftRecursion = Set.empty[Parser[_]]
    val mustConsumeCache = new ConsumeCache

    val reverseGraph = mutable.HashMap.empty[Parser2[_], mutable.Set[Parser2[_]]]
    GraphAlgorithms.depthFirst[Parser2[_]](root,
      node => {
        node.asInstanceOf[ParserBase[Any]].mustConsume = mustConsumeCache(node)
        node.children
      },
      (_, path: List[Parser2[_]]) => path match {
        case child :: parent :: _ =>
          val incoming = reverseGraph.getOrElseUpdate(child, mutable.HashSet.empty)
          incoming.add(parent)
        case _ =>
      },
      cycle => {
        nodesThatShouldDetectLeftRecursion += cycle.head
      })

    GraphAlgorithms.depthFirst[Parser2[_]](root,
      node => {
        node.leftChildren
      },
      (_, path: List[Parser[_]]) => {},
      cycle => {
        nodesThatShouldDetectLeftRecursion += cycle.head
      })

    val components = SCC.scc[Parser2[_]](reverseGraph.keys.toSet, node => node.leftChildren.toSet)
    val nodesInCycle: Set[Parser2[_]] = components.filter(c => c.size > 1).flatten.toSet

    val nodesWithMultipleIncomingEdges: Set[Parser2[_]] = reverseGraph.filter(e => e._2.size > 1).keys.toSet
    val nodesWithIncomingCycleEdge: Set[Parser2[_]] = reverseGraph.filter(e => e._2.exists(parent => nodesInCycle.contains(parent))).keys.toSet
    val nodesThatShouldCache: Set[Parser2[_]] = nodesWithIncomingCycleEdge ++ nodesWithMultipleIncomingEdges

    nodesInCycle.foreach(n => n.asInstanceOf[ParserBase[Any]].staticCycle = true)
    val parseState = newParseState(root)
    for(parser <- reverseGraph.keys) {
      val base = parser.asInstanceOf[ParserBase[Any]]
      base.parse = getParse(parseState, base, nodesThatShouldCache(parser), nodesThatShouldDetectLeftRecursion(parser))
    }
    parseState
  }

  val cache: TrieMap[Class[_], List[Parser[_] => Parser[_]]] = TrieMap.empty

  def getChildProperties(clazz: Class[Parser[_]]): List[Parser[_] => Parser[_]] = {
    cache.getOrElseUpdate(clazz, new ExtendedType[Parser[_]](clazz).fieldsOfType[Parser[_]](classOf[Parser[_]]))
  }

  class ConsumeCache {
    var values = mutable.Map.empty[Parser2[Any], Boolean]

    def apply[Result](parser: Parser2[Any]): Boolean = {
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

  implicit class ParserExtensions2[+Result](parser: Self[Result]) extends ParserExtensions(parser) {

    def addAlternative[Other >: Result](getAlternative: (Self[Other], Self[Other]) => Self[Other]): Self[Other] = {
      lazy val result: Self[Other] = lazyParser(parser | getAlternative(parser, result))
      result
    }

    def parseRoot(input: Input): ParseResult[Result] = {
      compile(parser)
      parser.parse(input)
    }
  }
}
