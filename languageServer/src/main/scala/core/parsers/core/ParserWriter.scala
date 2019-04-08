package core.parsers.core

import util.ExtendedType

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.language.higherKinds

trait LeftRecursiveParserWriter {

}

trait ParserWriter {

  type Input <: ParseInput
  type ParseResult[+Result] <: ParseResultLike[Result]
  type Self[+R] <: Parser[R]
  type ParseState

  def succeed[Result](result: Result): Self[Result]
  def newSuccess[Result](result: Result, remainder: Input): ParseResult[Result]
  def fail[Result](message: String): Self[Result]
  def lazyParser[Result](inner: => Self[Result]): Self[Result]

  def abort: ParseResult[Nothing]
  def newFailure[Result](input: Input, message: String): ParseResult[Result]

  def choice[Result](first: Self[Result], other: => Self[Result], leftIsAlwaysBigger: Boolean = false): Self[Result]

  def map[Result, NewResult](original: Self[Result], f: Result => NewResult): Self[NewResult]
    //= flatMap(original, (result: Result) => succeed(f(result)))

  def newParseState(root: Self[_]): ParseState
  def getParse[Result](parseState: ParseState,
                       parser: ParserBase[Result],
                       shouldCache: Boolean, shouldDetectLeftRecursion: Boolean): Parse[Result]

  def leftRight[Left, Right, NewResult](left: Self[Left],
                                        right: => Self[Right],
                                        combine: (Left, Right) => NewResult): Self[NewResult]

  implicit class ParserExtensions[+Result](parser: Self[Result]) {

    def parseRoot(input: Input): ParseResult[Result] = {
      compile(parser)
      parser.parse(input)
    }

    def addAlternative[Other >: Result](getAlternative: (Self[Other], Self[Other]) => Self[Other]): Self[Other] = {
      lazy val result: Self[Other] = lazyParser(parser | getAlternative(parser, result))
      result
    }

    def |[Other >: Result](other: => Self[Other]) = choice(parser, other)

    def ~[Right](right: => Self[Right]) = leftRight(parser, right, (a: Result, b: Right) => (a, b))

    def ~<[Right](right: Self[Right]) = leftRight(parser, right, Processor.ignoreRight[Result, Right])

    def ~>[Right](right: Self[Right]) = leftRight(parser, right, Processor.ignoreLeft[Result, Right])

    def map[NewResult](f: Result => NewResult): Self[NewResult] = ParserWriter.this.map(parser, f)

    def option: Self[Option[Result]] = choice(this.map(x => Some(x)), succeed[Option[Result]](None))

    def repN(amount: Int): Self[List[Result]] = {
      if (amount == 0) {
        succeed(List.empty[Result])
      } else {
        leftRight[Result, List[Result], List[Result]](parser, repN(amount - 1), (a,b) => a :: b)
      }
    }

    def many[Sum](zero: Sum, reduce: (Result, Sum) => Sum): Self[Sum] = {
      lazy val result: Self[Sum] = choice(leftRight(parser, result, reduce), succeed(zero), leftIsAlwaysBigger = true)
      result
    }

    def * : Self[List[Result]] = {
      many(List.empty, (h: Result, t: List[Result]) => h :: t)
    }

    def ^^[NewResult](f: Result => NewResult) = map(f)

    def manySeparated(separator: Self[Any]): Self[List[Result]] =
      leftRight(parser, (separator ~> parser).*, (h: Result, t: List[Result]) => h :: t) |
        succeed(List.empty[Result])
  }

  trait Parse[+Result] {
    def apply(input: Input): ParseResult[Result]
  }

  trait Parser[+Result] extends Parse[Result] {
    def apply(input: Input): ParseResult[Result]
    def parse: Parse[Result]
    def mustConsume: Boolean
    def getMustConsume(cache: ConsumeCache): Boolean
    def leftChildren: List[Parser[_]]
    def children: List[Parser[_]]
  }

  trait ParserBase[Result] extends Parser[Result] {
    var parse: Parse[Result] = this
    var staticCycle: Boolean = false
    var mustConsume: Boolean = false
  }

  trait SequenceLike[+Result] extends Parser[Result] {
    def left: Parser[Any]
    def right: Parser[Any]

    override def children = List(left, right)

    override def leftChildren = if (left.mustConsume) List(left) else List(left, right)

    override def getMustConsume(cache: ConsumeCache) = cache(left) || cache(right)
  }

  trait ChoiceLike[+Result] extends Parser[Result] {
    def first: Parser[Result]
    def second: Parser[Result]

    override def children = List(first, second)

    override def leftChildren = List(first, second)

    override def getMustConsume(cache: ConsumeCache) = cache(first) && cache(second)
  }


  trait ParserWrapper[+Result] extends Parser[Result] {
    def original: Parser[Any]

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

  case class Success[+Result](result: Result, remainder: Input) {
    def map[NewResult](f: Result => NewResult): Success[NewResult] = Success(f(result), remainder)
  }

  trait ParseResultLike[+Result] {
    def map[NewResult](f: Result => NewResult): ParseResult[NewResult] = {
      flatMap(s => newSuccess(f(s.result), s.remainder))
    }

    def flatMap[NewResult](f: Success[Result] => ParseResult[NewResult]): ParseResult[NewResult]
    def successful: Boolean
    def resultOption: Option[Result]
    def get = resultOption.get
  }

  trait LeafParser[+Result] extends Parser[Result] {
    override def leftChildren = List.empty

    override def children = List.empty
  }

  case class MapParser[Result, NewResult](original: Self[Result], f: Result => NewResult)
    extends ParserBase[NewResult]
    with ParserWrapper[NewResult] {

    override def apply(input: Input) = {
      val result = original.parse(input)
      result.map(f)
    }

    override def leftChildren = List(original)

    override def getMustConsume(cache: ConsumeCache) = cache(original)
  }

  case class Compile(shouldDetectLeftRecursion: Set[Parser[_]], nodesThatShouldCache: Set[Parser[_]])

  def compile[Result](root: Self[Result]): ParseState = {
    var nodesThatShouldDetectLeftRecursion = Set.empty[Parser[_]]
    val mustConsumeCache = new ConsumeCache

    val reverseGraph = mutable.HashMap.empty[Parser[_], mutable.Set[Parser[_]]]
    GraphAlgorithms.depthFirst[Parser[_]](root,
      node => {
        node.asInstanceOf[ParserBase[Any]].mustConsume = mustConsumeCache(node)
        node.children
      },
      (_, path: List[Parser[_]]) => path match {
        case child :: parent :: _ =>
          val incoming = reverseGraph.getOrElseUpdate(child, mutable.HashSet.empty)
          incoming.add(parent)
        case _ =>
      },
      cycle => {
        nodesThatShouldDetectLeftRecursion += cycle.head
      })

    GraphAlgorithms.depthFirst[Parser[_]](root,
      node => {
        node.leftChildren
      },
      (_, path: List[Parser[_]]) => {},
      cycle => {
        nodesThatShouldDetectLeftRecursion += cycle.head
      })

    val components = SCC.scc[Parser[_]](reverseGraph.keys.toSet, node => node.leftChildren.toSet)
    val nodesInCycle: Set[Parser[_]] = components.filter(c => c.size > 1).flatten.toSet

    val nodesWithMultipleIncomingEdges: Set[Parser[_]] = reverseGraph.filter(e => e._2.size > 1).keys.toSet
    val nodesWithIncomingCycleEdge: Set[Parser[_]] = reverseGraph.filter(e => e._2.exists(parent => nodesInCycle.contains(parent))).keys.toSet
    val nodesThatShouldCache: Set[Parser[_]] = nodesWithIncomingCycleEdge ++ nodesWithMultipleIncomingEdges // TODO investigate reducing the amount of nodes to cache, like subtracting nodes that are already cycles.

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
    var values = mutable.Map.empty[Parser[Any], Boolean]

    def apply[Result](parser: Parser[Any]): Boolean = {
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
}

object Processor {
  def ignoreLeft[Left, Right](left: Left, right: Right): Right = right
  def ignoreRight[Left, Right](left: Left, right: Right): Left = left
}

trait ParseInput {
  def offset: Int
  def atEnd: Boolean
}
