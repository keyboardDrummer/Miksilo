package core.parsers.core

import core.bigrammar.grammars.Parse
import util.cache.Cache

import scala.collection.mutable
import scala.language.higherKinds

trait ParserWriters {

  type Input <: ParseInput
  type ProcessResult[+ _] <: ParseResultLike[Input, _]
  type PN = ParseNode
  type Self[+R] <: Parser[R]
  type PState <: ParseState

  case class ParseNode(input: Input, parser: Parser[Any])

  def succeed[Result](result: Result): Self[Result]

  def fail[R](input: Input, message: String): ProcessResult[R]

  def choice[Result](first: Self[Result], other: => Self[Result], leftIsAlwaysBigger: Boolean = false): Self[Result]

  def flatMap[Result, NewResult](left: Self[Result], getRight: Result => Self[NewResult]): Self[NewResult]

  def map[Result, NewResult](original: Self[Result], f: Result => NewResult): Self[NewResult] =
    flatMap(original, (result: Result) => succeed(f(result)))

  def leftRight[Left, Right, NewResult](left: Self[Left],
                                        right: => Self[Right],
                                        combine: (Left, Right) => NewResult): Self[NewResult] = {
    flatMap(left, (leftResult: Left) => map(right, (rightResult: Right) => combine(leftResult, rightResult)))
  }

  implicit class ParserExtensions[+Result](parser: Self[Result]) {
    def |[Other >: Result](other: => Self[Other]) = choice(parser, other)

    def ~[Right](right: => Self[Right]) = leftRight(parser, right, (a: Result, b: Right) => (a, b))

    def ~<[Right](right: Self[Right]) = leftRight(parser, right, Processor.ignoreRight[Result, Right])

    def ~>[Right](right: Self[Right]) = leftRight(parser, right, Processor.ignoreLeft[Result, Right])

    def map[NewResult](f: Result => NewResult): Self[NewResult] = ParserWriters.this.map(parser, f)

    def option: Self[Option[Result]] = choice(this.map(x => Some(x)), succeed[Option[Result]](None))

    def many[Sum](zero: Sum, reduce: (Result, Sum) => Sum): Self[Sum] = {
      lazy val result: Self[Sum] = choice(leftRight(parser, result, reduce), succeed(zero), true)
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
    def parseNaively(input: Input, state: PState): ProcessResult[Result]

    def parseCached(input: Input, state: PState): ProcessResult[Result] = {
      val node = ParseNode(input, this)
      state.resultCache.get(node).getOrElse({
        val value = parseIteratively(input, state)
        if (!state.parsersPartOfACycle.contains(this)) {
          state.resultCache.add(node, value)
        }
        value
      }).asInstanceOf[ProcessResult[Result]]
    }

    def parseIteratively(input: Input, state: PState): ProcessResult[Result] = {
      val node = ParseNode(input, this)
      state.getPreviousResult(node) match {
        case None =>
          state.withNodeOnStack(node, () => {
            var result = parseNaively(input, state)
            if (result.getSuccessRemainder.nonEmpty && state.parsersWithBackEdges.contains(this)) { // TODO don't use nonEmpty.
              result = growResult(node, this, result, state)
            }
            result
          })

        case Some(result) => result
      }
    }

    private def growResult[GR >: Result](node: PN, parser: Parser[GR], previous: ProcessResult[GR], state: PState): ProcessResult[GR] = {
      state.putIntermediate(node, previous)

      val nextResult: ProcessResult[GR] = parser.parseNaively(node.input, state)
      nextResult.getSuccessRemainder match {
        case Some(remainder) if remainder.offset > previous.getSuccessRemainder.get.offset =>
          growResult(node, parser, nextResult, state)
        case None =>
          state.removeIntermediate(node)
          previous
      }
    }
  }

  class ParseState(val resultCache: Cache[ParseNode, ProcessResult[Any]]) {

    type PR[+R] = ProcessResult[R]

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
          fail[Result](node.input, "Traversed back edge without a previous result")).
          asInstanceOf[ProcessResult[Result]])
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
    override def parseCached(input: Input, state: PState): ProcessResult[Result] = inner.parseCached(input, state)
    override def parseIteratively(input: Input, state: PState): ProcessResult[Result] = inner.parseIteratively(input, state)
    override def parseNaively(input: Input, state: PState): ProcessResult[Result] = inner.parseNaively(input, state)
  }
}

object Processor {
  def ignoreLeft[Left, Right](left: Left, right: Right): Right = right
  def ignoreRight[Left, Right](left: Left, right: Right): Left = left
}

trait ParseResultLike[Input <: ParseInput, +Result] {
  def getSuccessRemainder: Option[Input]
  def successful: Boolean = getSuccessRemainder.nonEmpty
}

trait ParseInput {
  def offset: Int
  def atEnd: Boolean
}