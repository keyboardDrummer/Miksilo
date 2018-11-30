package core.parsers

import util.cache.Cache

import scala.collection.mutable
import scala.language.higherKinds

trait Parsers {

  type Input <: ParseInput
  type ProcessResult[+ _] <: ParseResultLike[Input, _]
  type PN = ParseNode
  type Self[+R] <: Parser[R]

  case class ParseNode(input: Input, parser: Parser[Any])

  def leftRight[Left, Right, NewResult](left: Self[Left],
                                        right: => Self[Right],
                                        combine: (Left, Right) => NewResult): Self[NewResult]

  def succeed[NR](result: NR): Self[NR]

  def choice[Result](first: Self[Result], other: => Self[Result], leftIsAlwaysBigger: Boolean = false): Self[Result]

  def flatMap[Result, NewResult](left: Self[Result], f: Result => Self[NewResult]): Self[NewResult]

  implicit class ParserExtensions[+Result](parser: Self[Result]) {
    def |[Other >: Result](other: => Self[Other]) = choice(parser, other)

    def ~[Right](right: => Self[Right]) = leftRight(parser, right, (a: Result, b: Right) => (a, b))

    def ~<[Right](right: Self[Right]) = leftRight(parser, right, Processor.ignoreRight[Result, Right])

    def ~>[Right](right: Self[Right]) = leftRight(parser, right, Processor.ignoreLeft[Result, Right])

    def map[NewResult](f: Result => NewResult): Self[NewResult] = flatMap(parser, (r: Result) => succeed(f(r)))

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

    //  type ParseSuccess[+R] = core.parsers.ParseSuccess[Input, R]
    //  type ParseFailure[+R] = core.parsers.ParseFailure[Input, R]

    /**
      * When implementing, make sure that when returning a failure,
      * if this Parser's default has a value, then the failure must do so to.
      */
    def parseNaively(input: Input, state: ParseState): ProcessResult[Result]

    def parseCached(input: Input, state: ParseState): ProcessResult[Result] = {
      val node = ParseNode(input, this)
      state.resultCache.get(node).getOrElse({
        val value = parseIteratively(input, state)
        if (!state.parsersPartOfACycle.contains(this)) {
          state.resultCache.add(node, value)
        }
        value
      }).asInstanceOf[ProcessResult[Result]]
    }

    def parseIteratively(input: Input, state: ParseState): ProcessResult[Result] = {
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

    private def growResult[GR >: Result](node: PN, parser: Parser[GR], previous: ProcessResult[GR], state: ParseState): ProcessResult[GR] = {
      state.putIntermediate(node, previous)

      val nextResult: ProcessResult[GR] = parser.parseNaively(node.input, state)
      if (nextResult.successful && nextResult.remainder.offset > previous.remainder.offset) {
        growResult(node, parser, nextResult, state)
      } else {
        state.removeIntermediate(node)
        previous
      }
    }
  }

  class ParseState(val resultCache: Cache[ParseNode, ProcessResult[Any]]) {

    type PR[+R] = ProcessResult[R]

    val defaultCache = new DefaultCache()
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
          ParseFailure[Input, Result](None, node.input, "Traversed back edge without a previous result")).
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
    override def parseCached(input: Input, cache: ParseState): ProcessResult[Result] = inner.parseCached(input, cache)
    override def parseIteratively(input: Input, cache: ParseState): ProcessResult[Result] = inner.parseIteratively(input, cache)
    override def parseNaively(input: Input, cache: ParseState): ProcessResult[Result] = inner.parseNaively(input, cache)
  }
}

object Processor {
  def ignoreLeft[Left, Right](left: Left, right: Right): Right = right
  def ignoreRight[Left, Right](left: Left, right: Right): Left = left
}

trait ParseResultLike[Input <: ParseInput, +Result] {
  def successful: Boolean
  def remainder: Input
}

trait ParseInput {
  def offset: Int
  def atEnd: Boolean
}