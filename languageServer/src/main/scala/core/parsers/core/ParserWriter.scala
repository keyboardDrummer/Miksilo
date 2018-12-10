package core.parsers.core

import util.cache.Cache

import scala.collection.mutable
import scala.language.higherKinds

trait ParserWriter {

  type Input <: ParseInput
  type ParseResult[+Result] <: ParseResultLike[Result]
  type PN = ParseNode
  type Self[+R] <: Parser[R]
  type PState <: ParseState

  case class ParseNode(input: Input, parser: Parser[Any])

  def succeed[Result](result: Result): Self[Result]
  def fail[Result](message: String): Self[Result]
  def lazyParser[Result](inner: => Self[Result]): Self[Result]

  def failure[Result](input: Input, message: String): ParseResult[Result]

  def choice[Result](first: Self[Result], other: => Self[Result], leftIsAlwaysBigger: Boolean = false): Self[Result]

  def flatMap[Result, NewResult](left: Self[Result], getRight: Result => Self[NewResult]): Self[NewResult]

  def map[Result, NewResult](original: Self[Result], f: Result => NewResult): Self[NewResult]
    //= flatMap(original, (result: Result) => succeed(f(result)))

  def leftRight[Left, Right, NewResult](left: Self[Left],
                                        right: => Self[Right],
                                        combine: (Left, Right) => NewResult): Self[NewResult]
//  = {
//    flatMap(left, (leftResult: Left) => map(right, (rightResult: Right) => combine(leftResult, rightResult)))
//  }

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
        succeed(List.empty)
  }

  trait Parser[+Result] {
    def parseNaively(input: Input, state: PState): ParseResult[Result]

    def parseCached(input: Input, state: PState): ParseResult[Result] = {
      if (state.resultCache == null)
        return parseNaively(input, state) //TODO find a nicer way

      val node = ParseNode(input, this)
      state.resultCache.get(node).getOrElse({
        val value = parseIteratively(input, state)
        if (!state.parsersPartOfACycle.contains(this)) {
          state.resultCache.add(node, value)
        }
        value
      }).asInstanceOf[ParseResult[Result]]
    }

    def parseIteratively(input: Input, state: PState): ParseResult[Result] = {
      if (state.resultCache == null)
        return parseNaively(input, state) //TODO find a nicer way

      val node = ParseNode(input, this)
      state.getPreviousResult(node) match {
        case None =>
          state.withNodeOnStack(node, () => {
            var result = parseNaively(input, state)
            if (result.successful && state.parsersWithBackEdges.contains(this)) {
              result = growResult(node, this, result, state)
            }
            result
          })

        case Some(result) => result
      }
    }

    private def growResult[GR >: Result](node: PN, parser: Parser[GR], previous: ParseResult[GR], state: PState): ParseResult[GR] = {
      state.putIntermediate(node, previous)

      val nextResult: ParseResult[GR] = parser.parseNaively(node.input, state)
      nextResult.getSuccessRemainder match {
        case Some(remainder) if remainder.offset > previous.getSuccessRemainder.get.offset =>
          growResult(node, parser, nextResult, state)
        case _ =>
          state.removeIntermediate(node)
          previous
      }
    }
  }

  class ParseState(val resultCache: Cache[ParseNode, ParseResult[Any]]) {

    type PR[+R] = ParseResult[R]

    val recursionIntermediates = mutable.HashMap[PN, PR[Any]]()
    val callStackSet = mutable.HashSet[PN]()
    val callStack = mutable.Stack[Parser[Any]]()
    var parsersPartOfACycle: Set[Parser[Any]] = Set.empty
    val parsersWithBackEdges = mutable.HashSet[Parser[Any]]() //TODO possible this can be only the parsers.

    def putIntermediate(key: PN, value: PR[Any]): Unit = {
      recursionIntermediates.put(key, value)
    }

    def removeIntermediate(node: PN): Unit = {
      recursionIntermediates.remove(node)
    }

    def getPreviousResult[Result](node: PN): Option[PR[Result]] = {
      if (callStackSet.contains(node)) {
        parsersWithBackEdges.add(node.parser)
        val index = callStack.indexOf(node.parser)
        parsersPartOfACycle ++= callStack.take(index + 1)
        return Some(recursionIntermediates.getOrElse(node,
          failure[Result](node.input, "Traversed back edge without a previous result")).
          asInstanceOf[ParseResult[Result]])
      }
      None
    }

    def withNodeOnStack[T](node: PN, action: () => T): T = {
      callStackSet.add(node)
      callStack.push(node.parser)
      val result = action()
      callStackSet.remove(node)
      callStack.pop()
      result
    }
  }

  class Lazy[+Result](_inner: => Parser[Result]) extends Parser[Result] {
    lazy val inner: Parser[Result] = _inner

    // We skip caching and left-recursion handling on lazy by redirecting parseCaching to the inner.
    override def parseCached(input: Input, state: PState): ParseResult[Result] = inner.parseCached(input, state)
    override def parseIteratively(input: Input, state: PState): ParseResult[Result] = inner.parseIteratively(input, state)
    override def parseNaively(input: Input, state: PState): ParseResult[Result] = inner.parseNaively(input, state)
  }

  trait ParseResultLike[+Result] {
    def getSuccessRemainder: Option[Input]
    def successful: Boolean = getSuccessRemainder.nonEmpty
    def get: Result
    def map[NewResult](f: Result => NewResult): ParseResult[NewResult]
  }

  class MapParser[Result, NewResult](original: Parser[Result], f: Result => NewResult) extends Parser[NewResult] {
    override def parseNaively(input: Input, state: PState) = {
      val result = original.parseCached(input, state)
      result.map(f)
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