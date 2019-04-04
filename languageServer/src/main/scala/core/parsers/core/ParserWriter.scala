package core.parsers.core

import util.ExtendedType

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.language.higherKinds

trait ParserWriter {

  type Input <: ParseInput
  type ParseResult[+Result] <: ParseResultLike[Result]
  type Self[+R] <: Parser[R]
  type ExtraState

  def succeed[Result](result: Result): Self[Result]
  def newSuccess[Result](result: Result, remainder: Input): ParseResult[Result]
  def fail[Result](message: String): Self[Result]
  def lazyParser[Result](inner: => Self[Result]): Self[Result]

  def abort: ParseResult[Nothing]
  def newFailure[Result](input: Input, message: String): ParseResult[Result]

  def choice[Result](first: Self[Result], other: => Self[Result], leftIsAlwaysBigger: Boolean = false): Self[Result]

  def flatMap[Result, NewResult](left: Self[Result], getRight: Result => Self[NewResult]): Self[NewResult]

  def map[Result, NewResult](original: Self[Result], f: Result => NewResult): Self[NewResult]
    //= flatMap(original, (result: Result) => succeed(f(result)))

  def leftRight[Left, Right, NewResult](left: Self[Left],
                                        right: => Self[Right],
                                        combine: (Left, Right) => NewResult): Self[NewResult]

  implicit class ParserExtensions[+Result](parser: Self[Result]) {

    def addAlternative[Other >: Result](getAlternative: (Self[Other], Self[Other]) => Self[Other]): Self[Other] = {
      lazy val result: Self[Other] = lazyParser(parser | getAlternative(parser, result))
      result
    }

    def |[Other >: Result](other: => Self[Other]) = choice(parser, other)

    def ~[Right](right: => Self[Right]) = leftRight(parser, right, (a: Result, b: Right) => (a, b))

    def ~<[Right](right: Self[Right]) = leftRight(parser, right, Processor.ignoreRight[Result, Right])

    def ~>[Right](right: Self[Right]) = leftRight(parser, right, Processor.ignoreLeft[Result, Right])

    def flatMap[NewResult](getRight: Result => Self[NewResult]): Self[NewResult] =
      ParserWriter.this.flatMap(parser, getRight)

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

  trait Parser[+Result] {
    def parseInternal(input: Input, state: ParseStateLike): ParseResult[Result]
    def children: List[Self[_]]
  }

  trait ParseStateLike {
    def getParse[Result](parser: Parser[Result]): Input => ParseResult[Result]
    def extraState: ExtraState
  }

  class EmptyParseState(val extraState: ExtraState) extends ParseStateLike {

    override def getParse[Result](parser: Parser[Result]) = input => parser.parseInternal(input, this)
  }

  class Lazy[+Result](_inner: => Self[Result]) extends Parser[Result] {
    lazy val inner: Self[Result] = _inner

    override def parseInternal(input: Input, state: ParseStateLike): ParseResult[Result] = state.getParse(inner)(input)

    override def children = List(inner)
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

  class MapParser[Result, NewResult](original: Self[Result], f: Result => NewResult) extends Parser[NewResult] {
    override def parseInternal(input: Input, state: ParseStateLike) = {
      val result = state.getParse(original)(input)
      result.map(f)
    }

    override def children = List(original)
  }

  case class Compile(shouldDetectLeftRecursion: Set[Parser[_]], nodesThatShouldCache: Set[Parser[_]])

  def compile[Result](root: Self[Result]): Compile = {
    var nodesThatShouldDetectLeftRecursion = Set.empty[Parser[_]]
    val reverseGraph = mutable.HashMap.empty[Parser[_], mutable.Set[Parser[_]]]
    GraphAlgorithms.depthFirst[Parser[_]](root,
      node => {
        node.children
      },
      (firstVisit, path: List[Parser[_]]) => path match {
        case child :: parent :: _ =>
          val incoming = reverseGraph.getOrElseUpdate(child, mutable.HashSet.empty)
          incoming.add(parent)
        case _ =>
      },
      cycle => {
        nodesThatShouldDetectLeftRecursion += cycle.head
      })

    val components = SCC.scc[Parser[_]](reverseGraph.keys.toSet, node => node.children.toSet)
    var nodesInCycle: Set[Parser[_]] = components.flatten.toSet

    val nodesWithMultipleIncomingEdges: Set[Parser[_]] = reverseGraph.filter(e => e._2.size > 1).keys.toSet
    val nodesWithIncomingCycleEdge: Set[Parser[_]] = reverseGraph.filter(e => e._2.exists(parent => nodesInCycle.contains(parent))).keys.toSet
    var nodesThatShouldCache: Set[Parser[_]] = nodesWithIncomingCycleEdge ++ nodesWithMultipleIncomingEdges
    Compile(nodesThatShouldDetectLeftRecursion, nodesThatShouldCache)
  }

  val cache: TrieMap[Class[_], List[Parser[_] => Parser[_]]] = TrieMap.empty

  def getChildProperties(clazz: Class[Parser[_]]): List[Parser[_] => Parser[_]] = {
    cache.getOrElseUpdate(clazz, new ExtendedType[Parser[_]](clazz).fieldsOfType[Parser[_]](classOf[Parser[_]]))
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

trait NotCorrectingParserWriter extends ParserWriter {
  type Self[+Result] = Parser[Result]

  def succeed[Result](result: Result): Self[Result] = new SuccessParser(result)
  class SuccessParser[+Result](result: Result) extends Parser[Result] {
    override def parseInternal(input: Input, state: ParseStateLike) = newSuccess(result, input)

    override def children = List.empty
  }

}
