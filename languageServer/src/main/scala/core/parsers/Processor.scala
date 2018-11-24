package core.parsers

import util.cache.Cache

import scala.collection.mutable

trait Processors {

  type Input <: ParseInput
  type ProcessResult[+ _] <: ParseResultLike[Input, _]

  type PN = ParseNode

  case class ParseNode(input: Input, parser: Processor[Any])

  def leftRight[Left, Right, NewResult](left: Processor[Left],
                                        right: => Processor[Right],
                                        combine: (Left, Right) => NewResult): Processor[NewResult]

  def succeed[NR](result: NR): Processor[NR]

  def choice[Result](first: Processor[Result], other: => Processor[Result], leftIsAlwaysBigger: Boolean = false): Processor[Result]

  def flatMap[Result, NewResult](left: Processor[Result], f: Result => Processor[NewResult]): Processor[NewResult]

  trait Processor[+Result] extends HasGetDefault[Result] {

    //  type ParseSuccess[+R] = core.parsers.ParseSuccess[Input, R]
    //  type ParseFailure[+R] = core.parsers.ParseFailure[Input, R]

    /**
      * When implementing, make sure that when returning a failure,
      * if this Parser's default has a value, then the failure must do so to.
      */
    def parseNaively(input: Input, state: ParseState): ProcessResult[Result]

    //  def parseWholeInput(input: Input,
    //                      cache: Cache[ParseNode[Input], PR[Any]] = new InfiniteCache()):
    //    PR[Result] = {
    //
    //    parse(input, cache)
    //  }

    //  def parse(input: Input,
    //            cache: Cache[ParseNode[Input], PR[Any]] = new InfiniteCache()): PR[Result] = {
    //    val state = new ParseState(cache)
    //    parseIteratively(input, state) match {
    //      case success: ParseSuccess[Result] =>
    //        if (success.remainder.atEnd) success
    //        else {
    //          val failedSuccess = ParseFailure(Some(success.result), success.remainder, "Did not parse entire input")
    //          failedSuccess.getBiggest(success.biggestFailure)
    //        }
    //      case f => f
    //    }
    //  }

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
            if (result.success && state.parsersWithBackEdges.contains(this)) {
              result = growResult(node, this, result, state)
            }
            result
          })

        case Some(result) => result
      }
    }

    private def growResult[GR >: Result](node: PN, parser: Processor[GR], previous: ProcessResult[GR], state: ParseState): ProcessResult[GR] = {
      state.putIntermediate(node, previous)

      val nextResult: ProcessResult[GR] = parser.parseNaively(node.input, state)
      if (nextResult.success && nextResult.remainder.offset > previous.remainder.offset) {
        growResult(node, parser, nextResult, state)
      } else {
        state.removeIntermediate(node)
        previous
      }
    }

    def |[Other >: Result](other: => Processor[Other]) = choice(this, other)

    def ~[Right](right: => Processor[Right]) = leftRight(this, right, (a: Result, b: Right) => (a, b))

    def ~<[Right](right: Processor[Right]) = leftRight(this, right, Processor.ignoreRight)

    def ~>[Right](right: Processor[Right]) = leftRight(this, right, Processor.ignoreLeft)

    def map[NewResult](f: Result => NewResult): Processor[NewResult] = flatMap(this, r => succeed(f(r)))

    def option: Processor[Option[Result]] = choice(this.map(x => Some(x)), succeed[Option[Result]](None))

    //=======
    //  def ~[Right](right: => Parser[Right]) = new Sequence(this, right, (a: Result,b: Right) => (a,b))
    //  def ~<[Right](right: Parser[Right]) = new Sequence(this, right, (a: Result,b: Right) => a)
    //  def ~>[Right](right: Parser[Right]) = new Sequence(this, right, (a: Result,b: Right) => b)
    //  def |[Other >: Result](other: => Parser[Other]) = new OrElse[Input, Result, Other, Other](this, other)
    //  def |||[Other >: Result](other: => Parser[Other]) = new BiggestOfTwo[Input, Result, Other, Other](this, other)
    //  def map[NewResult](f: Result => NewResult) = new MapParser(this, f)
    //  def option: Parser[Option[Result]] = this.map(x => Some(x)) | Return[Input, Option[Result]](None)
    //  def flatMap[NewResult](f: Result => Parser[NewResult]) = new FlatMap(this, f)
    //  def filter[Other >: Result](predicate: Other => Boolean, getMessage: Other => String) = Filter(this, predicate, getMessage)
    //  def withDefault[Other >: Result](_default: Other): Parser[Other] = WithDefault[Input, Other](this, cache => Some(_default))
    //>>>>>>> errorParsing2

    def many[Sum](zero: Sum, reduce: (Result, Sum) => Sum): Processor[Sum] = {
      lazy val result: Processor[Sum] = choice(leftRight(this, result, reduce), succeed(zero), true)
      result
    }

    def * : Processor[List[Result]] = {
      many(List.empty, (h: Result, t: List[Result]) => h :: t)
    }

    def ^^[NewResult](f: Result => NewResult) = map(f)

    def manySeparated(separator: Processor[Any]): Processor[List[Result]] =
      leftRight(this, (separator ~>[Result] this).*, (h: Result, t: List[Result]) => h :: t) |
        succeed(List.empty)

    final def getDefault(state: ParseState): Option[Result] = getDefault(state.defaultCache)
  }

  class ParseState(val resultCache: Cache[ParseNode, ProcessResult[Any]]) {

    type PR[+R] = ProcessResult[R]

    val defaultCache = new DefaultCache()
    val recursionIntermediates = mutable.HashMap[PN, PR[Any]]()
    val callStackSet = mutable.HashSet[PN]()
    val callStack = mutable.Stack[Processor[Any]]()
    var parsersPartOfACycle: Set[Processor[Any]] = Set.empty
    val parsersWithBackEdges = mutable.HashSet[Processor[Any]]() //TODO possible this can be only the parsers.

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

}

object Processor {
  def ignoreLeft[Left, Right](left: Left, right: Right): Right = right
  def ignoreRight[Left, Right](left: Left, right: Right): Left = left
//=======
//    new Sequence(this, (separator ~> this)*, (h: Result, t: List[Result]) => h :: t) |
//      Return(List.empty)
//
//  def withRange[Other >: Result](addRange: (Input, Input, Result) => Other): Parser[Other] = {
//    val withPosition = new Sequence(
//      new PositionParser[Input](),
//      new WithRemainderParser(this),
//      (left: Input, resultRight: (Result, Input)) => addRange(left, resultRight._2, resultRight._1))
//    WithDefault(withPosition, cache => this.getDefault(cache))
//  }
//>>>>>>> errorParsing2
}

trait ParseResultLike[Input <: ParseInput, Result] {
  def success: Boolean
  def remainder: Input
}

trait ParseInput {
  def offset: Int
  def atEnd: Boolean
}