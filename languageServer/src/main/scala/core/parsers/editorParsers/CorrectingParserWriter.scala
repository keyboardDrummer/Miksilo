package core.parsers.editorParsers

import core.parsers.core.{OptimizingParserWriter, ParseInput}

trait CorrectingInput extends ParseInput {
  def offsetScore: Int
}

trait CorrectingParserWriter extends OptimizingParserWriter with EditorParserWriter {

  type Input <: CorrectingInput

  def parse[Result](parser: EditorParser[Result], input: Input): ParseWholeResult[Result] = {

    var bestResult: ReadyParseResult[Result] =
      ReadyParseResult(None, input, List(ParseError(input, "Grammar is always recursive", Int.MaxValue)))

    var queue = parser.parseRoot(input)
    while(true) {
      val (parseResult, tail) = queue.pop()
      queue = parseResult match {
        case parseResult: ReadyParseResult[Result] =>
          bestResult = if (bestResult != null && bestResult.score >= parseResult.score) bestResult else parseResult
          if (bestResult.remainder.atEnd)
            Empty
          else
            tail
        case delayedResult: DelayedParseResult[Result] =>
          val results = delayedResult.results
          tail.merge(results)
      }
    }
    ParseWholeResult(bestResult.resultOption, bestResult.errors)
  }

  def singleResult[Result](parseResult: LazyParseResult[Result]) = Cons(parseResult, Empty)

  type ParseResult[+Result] = SortedParseResults[Result]

  override def newFailure[Result](partial: Option[Result], input: Input, errors: List[ParseError]) =
    singleResult(ReadyParseResult(partial, input, errors))

  override def newSuccess[Result](result: Result, remainder: Input) = singleResult(ReadyParseResult(Some(result), remainder, List.empty))

  def newFailure[Result](input: Input, message: String) = singleResult(ReadyParseResult(None, input, List(ParseError(input, message))))

  override def leftRight[Left, Right, NewResult](left: EditorParser[Left],
                                                 right: => EditorParser[Right],
                                                 combine: (Left, Right) => NewResult): EditorParser[NewResult] =
    new Sequence(left, right, combine)

  override def succeed[NR](result: NR): EditorParser[NR] = Succeed(result)

  override def choice[Result](first: EditorParser[Result], other: => EditorParser[Result], leftIsAlwaysBigger: Boolean): EditorParser[Result] =
  /*if (leftIsAlwaysBigger) new OrElse(first, other) else*/ new BiggestOfTwo(first, other)

  override def map[Result, NewResult](original: Self[Result], f: Result => NewResult): Self[NewResult] = new MapParser(original, f)

  override def lazyParser[Result](inner: => EditorParser[Result]) = new EditorLazy(inner)

  type PopResult = (LazyParseResult[Any], SortedParseResults[Any])
  type ResultStack = List[PopResult]
  trait PopFunction {
    def pop(functionStack: FunctionStack, resultStack: ResultStack): (FunctionStack, ResultStack)
  }
  type FunctionStack = List[PopFunction]

  sealed trait SortedParseResults[+Result] extends ParseResultLike[Result]  {

    def function: PopFunction

    def pop(): (LazyParseResult[Result], SortedParseResults[Result]) = {
      var resultStack: ResultStack = List.empty
      var functionStack: FunctionStack = List(this.function)

      while(functionStack.nonEmpty) {
        val result :: tail = functionStack
        val n = result.pop(tail, resultStack)
        functionStack = n._1
        resultStack = n._2
      }

      resultStack.head.asInstanceOf[(LazyParseResult[Result], SortedParseResults[Result])]
    }

    def depth: Int
    def merge[Other >: Result](other: SortedParseResults[Other]): SortedParseResults[Other] = Merge(this, other)

    override def map[NewResult](f: Result => NewResult): SortedParseResults[NewResult] = SRMap(this, f)

    def mapResult[NewResult](f: LazyParseResult[Result] => LazyParseResult[NewResult]): SortedParseResults[NewResult] =
    {
      flatMap(r => singleResult(f(r)))
    }

    def flatMap[NewResult](f: LazyParseResult[Result] => SortedParseResults[NewResult]): SortedParseResults[NewResult] = {
      FlatMap(this, f)
    }

    def addErrors(errors: List[ParseError]): SortedParseResults[Result] = {
      mapWithErrors(x => x, errors)
    }

    def mapWithErrors[NewResult](f: ReadyParseResult[Result] => ReadyParseResult[NewResult],
                                 oldErrors: List[ParseError]): SortedParseResults[NewResult] = {
      mapResult({
        case delayed: DelayedParseResult[Result] => new DelayedParseResult(delayed.remainder, delayed.errors ++ oldErrors, () => {
          val intermediate = delayed.results
          intermediate.mapWithErrors(f, oldErrors)
        })
        case ready: ReadyParseResult[Result] =>
          val newReady = f(ready)
          ReadyParseResult(newReady.resultOption, newReady.remainder, newReady.errors ++ oldErrors)
      })
    }

    def mapReady[NewResult](f: ReadyParseResult[Result] => ReadyParseResult[NewResult]): SortedParseResults[NewResult] = {
      mapResult({
        case delayed: DelayedParseResult[Result] => new DelayedParseResult(delayed.remainder, delayed.errors, () => {
            val intermediate = delayed.results
            intermediate.mapReady(f)
          })
        case ready: ReadyParseResult[Result] => f(ready)
      })
    }

    def flatMapReady[NewResult](f: ReadyParseResult[Result] => SortedParseResults[NewResult]): SortedParseResults[NewResult] = {
      flatMap[NewResult] {
        case delayed: DelayedParseResult[Result] => singleResult(new DelayedParseResult(delayed.remainder, delayed.errors, () => {
          val intermediate = delayed.results
          intermediate.flatMapReady(f)
        }))
        case ready: ReadyParseResult[Result] => f(ready)
      }
    }

    def updateRemainder(f: Input => Input) = {
      mapReady(r => ReadyParseResult(r.resultOption, f(r.remainder), r.errors))
    }
  }

  object Empty extends SortedParseResults[Nothing] {
    override def function = (a,b) => (a, b)

    override def depth = 0
  }

  case class Cons[Result](head: LazyParseResult[Result], tail: SortedParseResults[Result]) extends SortedParseResults[Result] {
    override def function: PopFunction = (functionStack: FunctionStack, resultStack: ResultStack) => {
      val applyFunction: PopFunction = (functionStack2: FunctionStack, resultStack2: ResultStack) => {
        val resultStack3: ResultStack = resultStack2 match {
          case tailResult :: resultTail => (head, Cons(tailResult._1, tailResult._2)) :: resultTail
          case _ =>
            List(head -> Empty)
        }
        (functionStack2, resultStack3)
      }
      (tail.function :: applyFunction :: functionStack, resultStack)
    }

    override def depth = 1 + tail.depth
  }

  case class Merge[Result](left: SortedParseResults[Result], right: SortedParseResults[Result]) extends SortedParseResults[Result] {
    override def depth = 1 + Math.max(left.depth, right.depth)

    override def function: PopFunction = (functionStack: FunctionStack, resultStack) => {
      val merge: PopFunction = (functionStack2: FunctionStack, resultStack2: ResultStack) => {
        val resultStack3 = resultStack2 match {
          case leftResult :: rightResult :: resultTail =>
            val leftHead = leftResult._1
            val rightHead = rightResult._1
            val newResult: PopResult = if (leftHead.score > rightHead.score) {
              (leftHead, Merge(leftResult._2, Cons(rightHead, rightResult._2)))
            } else
              (rightHead, Merge(rightResult._2, Cons(leftHead, leftResult._2)))
            newResult :: resultTail
          case _ => resultStack2
        }
        (functionStack2, resultStack3)
      }

      (left.function :: right.function :: merge :: functionStack, resultStack)
    }
  }

  case class FlatMap[Result, +NewResult](original: SortedParseResults[Result], f: LazyParseResult[Result] => SortedParseResults[NewResult])
    extends SortedParseResults[NewResult] {
    override def depth = original.depth + 1

    override def function: PopFunction = (functionStack: FunctionStack, resultStack) => {
      val applyFunction: PopFunction = (functionStack2: FunctionStack, resultStack2) => {
        val head :: newResults = resultStack2
        (Merge[NewResult](
          f(head._1.asInstanceOf[LazyParseResult[Result]]),
          head._2.asInstanceOf[SortedParseResults[NewResult]]).function :: functionStack2, newResults)
      }
      (original.function :: applyFunction :: functionStack, resultStack)
    }
  }

  case class SRMap[Result, +NewResult](original: SortedParseResults[Result],
                                      f: Result => NewResult)
    extends SortedParseResults[NewResult] {
    override def depth = original.depth + 1

    override def function: PopFunction = (functionStack: FunctionStack, resultStack) => {
      val applyFunction: PopFunction = (functionStack2: FunctionStack, resultStack2) => {
        val head :: newResults = resultStack2
        (functionStack2, head._1.asInstanceOf[LazyParseResult[Result]].map(f) -> head._2 :: newResults)
      }
      (original.function :: applyFunction :: functionStack, resultStack)
    }

  }

//  final class SRCons[+Result](val head: LazyParseResult[Result], var depth: Int, _tail: => SortedParseResults[Result]) extends SortedParseResults[Result] {
//
//    if (depth > 200) {
//      throw new Exception("too deep")
//    }
//    if (depth > 100) {
//      if (tail.depth > -1)
//        depth = 0
//      else
//        depth = 0
//    }
//    //    // Detect incorrect ordering.
//    //
//    //    val score = head.score
//
//    lazy val tail = _tail
//    // Detect multiple access of tail
//    //    var switch = true
//    //    def tail = {
//    //      if (switch) {
//    //        switch = false
//    //        val result = _tail
//    //        result
//    //      }
//    //      else {
//    //        ???
//    //      }
//    //    }
//
//    override def mapResult[NewResult](f: LazyParseResult[Result] => LazyParseResult[NewResult]) = {
//      flatMap(r => singleResult(f(r)))
//    }
//
//    def flatMap[NewResult](f: LazyParseResult[Result] => SortedParseResults[NewResult]): SortedParseResults[NewResult] = {
//      f(head).merge(tail.flatMap(f))
//    }
//
//    override def map[NewResult](f: Result => NewResult): SRCons[NewResult] = {
//      new SRCons(head.map(f), depth + 1, tail.map(f))
//    }
//
//    @tailrec
//    def merge2(other: SortedParseResults[Result], results: mutable.ArrayBuffer[LazyParseResult[Result]]): SortedParseResults[Result] = {
//      var left = this
//      var right = other
//      other match {
//        case other: SRCons[Result] => if (head.score >= other.head.score) {
//          new SRCons(head, 1 + Math.max(tail.depth, other.depth), )
//          tail.merge(other)
//        } else
//          results.+=(other.head)
//          this.merge(other.tail)
//      }
//    }
//
//    override def merge[Other >: Result](other: SortedParseResults[Other]): SortedParseResults[Other] = {
//      var left = this
//      var right = other
//      other match {
//        case SREmpty => this
//        case other: SRCons[Other] => if (head.score >= other.head.score) {
//          new SRCons(head, 1 + Math.max(tail.depth, other.depth), tail.merge(other))
//        } else
//          new SRCons(other.head, 1 + Math.max(this.depth, other.depth), this.merge(other.tail))
//      }
//    }
//  }

  trait LazyParseResult[+Result] {

    val score: Double =  {
      val result =
        // -errorSize // gives us the most correct result, but can be very slow
        remainder.offsetScore - 6  * errorSize // gets us to the end the fastest. the 5 is because sometimes a single incorrect insertion can lead to some offset gain.
        // remainder.offset / (errorSize + 1) // compromise
      result // result + (if (errorSize == 0) 100 else 0) // This is so that for correct inputs, we only need a single iteration.
    }

    def errorSize = errors.map(e => e.edits).sum
    def errors: List[ParseError]
    def remainder: Input
    def map[NewResult](f: Result => NewResult): LazyParseResult[NewResult]
  }

  class DelayedParseResult[Result](val remainder: Input, val errors: List[ParseError], _getResults: () => SortedParseResults[Result])
    extends LazyParseResult[Result] {

    if (errors.isEmpty) {
      System.out.append("")
    }

    override def map[NewResult](f: Result => NewResult): DelayedParseResult[NewResult] = {
      new DelayedParseResult(remainder, errors, () => results.map(f))
    }

    lazy val results: SortedParseResults[Result] = _getResults()
  }

  case class ReadyParseResult[+Result](resultOption: Option[Result], remainder: Input, errors: List[ParseError])
    extends LazyParseResult[Result] {

    override def map[NewResult](f: Result => NewResult): ReadyParseResult[NewResult] = {
      ReadyParseResult(resultOption.map(f), remainder, errors)
    }
  }

  class Sequence[+Left, +Right, Result](val left: EditorParser[Left],
                                         _right: => EditorParser[Right],
                                         combine: (Left, Right) => Result)
    extends EditorParserBase[Result] with SequenceLike[Result] {

    lazy val right: EditorParser[Right] = _right

    override def getParser(recursive: GetParse): Parse[Result] = {
      val parseLeft = recursive(left)
      lazy val parseRight = recursive(right)

      new Parse[Result] {
        override def apply(input: Input, state: ParseState) = {
          val leftResults = parseLeft(input, state)

          def rightFromLeftReady(delayOnError: Boolean): ReadyParseResult[Left] => SortedParseResults[Result] =
            (ready: ReadyParseResult[Left]) => {
              def mapRightResult(rightResult: ReadyParseResult[Right]): ReadyParseResult[Result] = ReadyParseResult(
                ready.resultOption.flatMap(l => rightResult.resultOption.map(r => combine(l, r))),
                rightResult.remainder,
                rightResult.errors)

              if (delayOnError && ready.errors.nonEmpty)
                singleResult(new DelayedParseResult(ready.remainder, ready.errors,
                  () => parseRight(ready.remainder, state).mapWithErrors[Result](mapRightResult, ready.errors)))
              else
                parseRight(ready.remainder, state).mapWithErrors[Result](mapRightResult, ready.errors)
            }

          leftResults.flatMap({
            case ready: ReadyParseResult[Left] => rightFromLeftReady(true)(ready)
            case delayed: DelayedParseResult[Left] =>

              singleResult(new DelayedParseResult[Result](delayed.remainder, delayed.errors,
                () => {
                  val intermediate = delayed.results
                  intermediate.flatMapReady(rightFromLeftReady(false))
                }))
          })
        }
      }
    }

    override def getDefault(cache: DefaultCache): Option[Result] = for {
      leftDefault <- cache(left)
      rightDefault <- cache(right)
    } yield combine(leftDefault, rightDefault)
  }


  implicit class EditorParserExtensions[Result](parser: EditorParser[Result]) extends ParserExtensions(parser) {

    def filter[Other >: Result](predicate: Other => Boolean, getMessage: Other => String) = Filter(parser, predicate, getMessage)

    def withDefault[Other >: Result](_default: Other): EditorParser[Other] =
      this | Fail(Some(_default), "a default")

    def parseWholeInput(input: Input): ParseWholeResult[Result] = {
      CorrectingParserWriter.this.parseWholeInput(parser, input)
    }

    override def parseRoot(input: Input): ParseResult[Result] = {
      setDefaults(parser)
      val analysis = compile(parser)
      analysis.getParse(parser)(input, newParseState(input))
    }

    def withRange[Other >: Result](addRange: (Input, Input, Result) => Other): EditorParser[Other] = {
      val withPosition = leftRight(
        PositionParser,
        WithRemainderParser(parser),
        (left: Input, resultRight: Success[Result]) => addRange(left, resultRight.remainder, resultRight.result))
      withPosition // WithDefault(withPosition, cache => parser.getDefault(cache))
    }
  }

  class BiggestOfTwo[+First <: Result, +Second <: Result, Result](val first: EditorParser[First], _second: => EditorParser[Second])
    extends EditorParserBase[Result] with ChoiceLike[Result] {

    lazy val second = _second

    override def getParser(recursive: GetParse): Parse[Result] = {
      val parseFirst = recursive(first)
      lazy val parseSecond = recursive(second)

      new Parse[Result] {
        override def apply(input: Input, state: ParseState) = {
          val firstResult = parseFirst(input, state)
          val secondResult = parseSecond(input, state)
          firstResult.merge(secondResult)
        }
      }
    }

    override def getDefault(cache: DefaultCache): Option[Result] = {
      val value: Option[First] = cache(first)
      value.orElse(cache(second))
    }
  }

  case class Filter[Other, Result <: Other](original: EditorParser[Result],
                                            predicate: Other => Boolean, getMessage: Other => String)
    extends EditorParserBase[Result] with ParserWrapper[Result] {


    override def getParser(recursive: GetParse): Parse[Result] = {
      val parseOriginal = recursive(original)
      (input, state) => {
        val originalResult = parseOriginal(input, state)
        originalResult.mapReady(s => {
          s.resultOption match {
            case Some(result) => if (predicate(result)) s else ReadyParseResult(default, s.remainder,
              ParseError(s.remainder, getMessage(result)) :: s.errors)
            case None => s
          }
        })
      }
    }

    override def getDefault(cache: DefaultCache): Option[Result] =
      original.getDefault(cache).filter(predicate)
  }

  case class WithRemainderParser[Result](original: Self[Result])
    extends EditorParserBase[Success[Result]] with ParserWrapper[Success[Result]] {

    override def getParser(recursive: GetParse): Parse[Success[Result]] = {
      val parseOriginal = recursive(original)
      (input, state) => parseOriginal(input, state).mapReady(r =>
        ReadyParseResult(r.resultOption.map(v => Success(v, r.remainder)), r.remainder, r.errors))
    }

    override def getDefault(cache: DefaultCache): Option[Success[Result]] = None
  }

}
