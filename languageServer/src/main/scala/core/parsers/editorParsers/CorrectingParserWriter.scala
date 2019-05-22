package core.parsers.editorParsers

import core.language.node.SourceRange
import core.parsers.core.OptimizingParserWriter

trait CorrectingParserWriter extends OptimizingParserWriter with EditorParserWriter {

  def parse[Result](parser: Self[Result], input: Input, mayStop: () => Boolean): ParseWholeResult[Result] = {

    val noResultFound = ReadyParseResult(None, input, History.error(GenericError(input, "Grammar is always recursive", Int.MaxValue)))
    var bestResult: ReadyParseResult[Result] =
      noResultFound


    val start = System.currentTimeMillis()
    var queue = parser.parseRoot(input)
    while(queue.isInstanceOf[SRCons[Result]]) {
      val cons = queue.asInstanceOf[SRCons[Result]]
      val parseResult = cons.head

      queue = parseResult match {
        case parseResult: ReadyParseResult[Result] =>
          bestResult = if (bestResult.score >= parseResult.score) bestResult else parseResult
          cons.tail match {
            case tailCons: SRCons[Result] =>
              if (bestResult.originalScore > tailCons.head.score && mayStop())
                SREmpty
              else
                cons.tail
            case _ => SREmpty
          }
        case delayedResult: DelayedParseResult[Result] =>
          val results = delayedResult.results
          cons.tail.merge(results)
      }
    }
    System.out.append(s"Took: ${System.currentTimeMillis() - start} ms")
    ParseWholeResult(bestResult.resultOption, bestResult.history.errors.toList)
  }

  def singleResult[Result](parseResult: LazyParseResult[Result]) =
    new SRCons(parseResult,0, SREmpty)

  type ParseResult[+Result] = SortedParseResults[Result]

  override def newFailure[Result](partial: Option[Result], input: Input, errors: MyHistory) =
    singleResult(ReadyParseResult(partial, input, errors))

  def newSuccess[Result](result: Result, remainder: Input, score: Double): SRCons[Result] =
    singleResult(ReadyParseResult(Some(result), remainder, SpotlessHistory().addSuccess(remainder, remainder, result, score)))

  def newFailure[Result](input: Input, message: String): SRCons[Nothing] =
    singleResult(ReadyParseResult(None, input, History.error(GenericError(input, message, History.genericErrorPenalty))))

  override def leftRight[Left, Right, NewResult](left: Self[Left],
                                                 right: => Self[Right],
                                                 combine: (Left, Right) => NewResult): Self[NewResult] =
    new Sequence(left, right, combine)

  override def succeed[NR](result: NR): Self[NR] = Succeed(result)

  override def withDefault[Result](original: LRParser[Result], value: Result) = {
    WithDefault(original, value) | succeed(value)
  }

  override def choice[Result](first: Self[Result], other: => Self[Result]): Self[Result] = new BiggestOfTwo(first, other)

  override def map[Result, NewResult](original: Self[Result], f: Result => NewResult): Self[NewResult] = new MapParser(original, f)

  sealed trait SortedParseResults[+Result] extends ParseResultLike[Result]  {
    def toList: List[LazyParseResult[Result]]
    def tailDepth: Int
    def merge[Other >: Result](other: SortedParseResults[Other], depth: Int = 0): SortedParseResults[Other]
    def mapResult[NewResult](f: LazyParseResult[Result] => LazyParseResult[NewResult]): SortedParseResults[NewResult]
    def flatMap[NewResult](f: LazyParseResult[Result] => SortedParseResults[NewResult]): SortedParseResults[NewResult]

    def addHistory(errors: MyHistory): SortedParseResults[Result] = {
      mapWithHistory(x => x, errors)
    }

    def mapWithHistory[NewResult](f: ReadyParseResult[Result] => ReadyParseResult[NewResult],
                                  oldHistory: MyHistory): SortedParseResults[NewResult] = {
      mapResult(l => l.mapWithHistory(f, oldHistory))
    }

    def mapReady[NewResult](f: ReadyParseResult[Result] => ReadyParseResult[NewResult]): SortedParseResults[NewResult] = {
      mapResult(l => l.mapReady(f))
    }

    def flatMapReady[NewResult](f: ReadyParseResult[Result] => SortedParseResults[NewResult]): SortedParseResults[NewResult] = {
      flatMap[NewResult](l => l.flatMapReady(f))
    }

    def updateRemainder(f: Input => Input) = {
      mapReady(r => ReadyParseResult(r.resultOption, f(r.remainder), r.history))
    }
  }

  object SREmpty extends SortedParseResults[Nothing] {
    override def merge[Other >: Nothing](other: SortedParseResults[Other], depth: Int) = other

    override def mapResult[NewResult](f: LazyParseResult[Nothing] => LazyParseResult[NewResult]): SREmpty.type = this

    override def flatMap[NewResult](f: LazyParseResult[Nothing] => SortedParseResults[NewResult]) = this

    override def map[NewResult](f: Nothing => NewResult) = this

    override def tailDepth = 0

    override def toList = List.empty
  }

  final class SRCons[+Result](val head: LazyParseResult[Result],
                              var tailDepth: Int,
                              _tail: => SortedParseResults[Result]) extends SortedParseResults[Result] {

    // Used for debugging
    def toList: List[LazyParseResult[Result]] = head :: tail.toList

    def getTail = tail
    lazy val tail = _tail
    //tail

//    if (depth > 500) {
//      throw new Error()
//    }
//
    if (tailDepth == 50) {
      tail
      tailDepth = 0
    }

//    // Detect incorrect ordering.
//    val score = head.score
//    for(result <- results.drop(0)) {
//      if (result.score > score) {
//        throw new Exception("sorting was incorrect")
//      }
//    }

    // Detect multiple access of tail
//    var switch = true
//    def tail = {
//      if (switch) {
//        switch = false
//        val result = _tail
//        result
//      }
//      else {
//        ???
//      }
//    }

    override def mapResult[NewResult](f: LazyParseResult[Result] => LazyParseResult[NewResult]): SortedParseResults[NewResult] = {
      flatMap(r => singleResult(f(r)))
    }

    def flatMap[NewResult](f: LazyParseResult[Result] => SortedParseResults[NewResult]): SortedParseResults[NewResult] = {
      f(head) match {
        case SREmpty => tail.flatMap(f)
        case cons: SRCons[NewResult] =>

          if (head.score != cons.head.score)
            cons.merge(tail.flatMap(f))
          else
          {
            new SRCons(
              cons.head,
              1 + Math.max(this.tailDepth, cons.tailDepth),
              cons.tail.merge(tail.flatMap(f)))
          }
      }
    }

    override def map[NewResult](f: Result => NewResult): SRCons[NewResult] = {
      new SRCons(head.map(f), tailDepth + 1, tail.map(f))
    }

    override def merge[Other >: Result](other: SortedParseResults[Other], mergeDepth: Int): SortedParseResults[Other] = {
      if (mergeDepth > 1000) // Should be 200, since 100 is not enough to let CorrectionJsonTest.realLifeExample2 pass
        return SREmpty

      other match {
        case SREmpty => this
        case other: SRCons[Other] =>
          if (head.score >= other.head.score) {
            new SRCons(head,1 + Math.max(tailDepth, other.tailDepth), tail.merge(other, mergeDepth + 1))
          } else
            new SRCons(other.head,1 + Math.max(tailDepth, other.tailDepth), this.merge(other.tail, mergeDepth + 1))
      }
    }
  }

  trait LazyParseResult[+Result] {
    def flatMapReady[NewResult](f: ReadyParseResult[Result] => SortedParseResults[NewResult]): SortedParseResults[NewResult]

    def mapReady[NewResult](f: ReadyParseResult[Result] => ReadyParseResult[NewResult]): LazyParseResult[NewResult]

    val score: Double = (if (history.flawed) 0 else 10000) + history.score

    def history: MyHistory
    def map[NewResult](f: Result => NewResult): LazyParseResult[NewResult]

    def mapWithHistory[NewResult](f: ReadyParseResult[Result] => ReadyParseResult[NewResult],
                       oldHistory: MyHistory): LazyParseResult[NewResult]
  }

  class DelayedParseResult[Result](val history: MyHistory, _getResults: () => SortedParseResults[Result])
    extends LazyParseResult[Result] {

    if (!history.flawed) {
      System.out.append("")
      ???
    }

    override def toString = score + " delayed: " + history

    override def map[NewResult](f: Result => NewResult): DelayedParseResult[NewResult] = {
      new DelayedParseResult(history, () => results.map(f))
    }

    lazy val results: SortedParseResults[Result] = _getResults()

    override def mapWithHistory[NewResult](f: ReadyParseResult[Result] => ReadyParseResult[NewResult], oldHistory: MyHistory) =
      new DelayedParseResult(this.history ++ oldHistory, () => {
        val intermediate = this.results
        intermediate.mapWithHistory(f, oldHistory)
    })

    override def mapReady[NewResult](f: ReadyParseResult[Result] => ReadyParseResult[NewResult]): DelayedParseResult[NewResult] =
      new DelayedParseResult(this.history, () => {
        val intermediate = this.results
        intermediate.mapReady(f)
      })

    override def flatMapReady[NewResult](f: ReadyParseResult[Result] => SortedParseResults[NewResult]) =
      singleResult(new DelayedParseResult(this.history, () => {
        val intermediate = this.results
        intermediate.flatMapReady(f)
      }))
  }

  case class ReadyParseResult[+Result](resultOption: Option[Result], remainder: Input, history: MyHistory)
    extends LazyParseResult[Result] {

    val originalScore = (if (history.flawed) 0 else 10000) + history.score
    override val score = 1000000 + originalScore

    override def map[NewResult](f: Result => NewResult): ReadyParseResult[NewResult] = {
      ReadyParseResult(resultOption.map(f), remainder, history)
    }

    override def mapWithHistory[NewResult](f: ReadyParseResult[Result] => ReadyParseResult[NewResult], oldHistory: MyHistory) = {
      val newReady = f(this)
      ReadyParseResult(newReady.resultOption, newReady.remainder, newReady.history ++ oldHistory)
    }

    override def mapReady[NewResult](f: ReadyParseResult[Result] => ReadyParseResult[NewResult]): ReadyParseResult[NewResult] = f(this)

    override def flatMapReady[NewResult](f: ReadyParseResult[Result] => SortedParseResults[NewResult]) = f(this)
  }

  class Sequence[+Left, +Right, Result](val left: Self[Left],
                                         _right: => Self[Right],
                                         combine: (Left, Right) => Result,
                                        mayDrop: Boolean = true)
    extends EditorParserBase[Result] with SequenceLike[Result] {

    lazy val right: Self[Right] = _right

    override def getParser(recursive: GetParse): Parse[Result] = {
      val parseLeft = recursive(left)
      lazy val parseRight = recursive(right)

      new Parse[Result] {
        override def apply(input: Input, state: ParseState) = {
          val leftResults = parseLeft(input, state)

          val delayedLeftResults: SortedParseResults[Left] = leftResults.mapResult({
            case ready: ReadyParseResult[Left] =>
              if (ready.history.flawed)
                new DelayedParseResult[Left](ready.history, () => singleResult(ready))
              else
                ready
            case lazyResult => lazyResult
          })

          def rightFromLeftReady(leftReady: ReadyParseResult[Left]): SortedParseResults[Result] = {
            def mapRightResult(rightResult: ReadyParseResult[Right]): ReadyParseResult[Result] = ReadyParseResult(
              leftReady.resultOption.flatMap(l => rightResult.resultOption.map(r => combine(l, r))),
              rightResult.remainder,
              rightResult.history)

            if (leftReady.remainder.offset == 2 && leftReady.history.score == 2) {
              System.out.append("")
            }
            if (leftReady.remainder.offset == 241 && leftReady.history.score == 3) {
              System.out.append("")
            }
            val rightResult = parseRight(leftReady.remainder, state)
            rightResult.mapWithHistory[Result](mapRightResult, leftReady.history)
          }
          delayedLeftResults.flatMapReady(rightFromLeftReady)
        }
      }
    }
  }


  implicit class EditorParserExtensions[Result](parser: Self[Result]) extends ParserExtensions(parser) {

    // Drops suffix. It's nice to drop before the next | nil OR, so that you don't get duplicate drops. And if you drop before the nil, you get a suffix drop.
    def someSeparated(separator: Self[Any], elementName: String): Self[List[Result]] = {
      val reduce = (h: Result, t: List[Result]) => h :: t
      val zero = List.empty[Result]
      lazy val result: Self[List[Result]] = separator ~>
        (WithDefault(leftRight(DropParser(parser), DropParser(result), reduce), zero) |
          Fail(Some(zero), elementName, History.insertDefaultPenalty)) |
        succeed(zero)
      leftRight(parser, DropParser(result), reduce)
    }

    //Drops prefix and suffix.
    //Drop(succeed) creates a pre and suffix drop, and Drop(x) where x was already a suffix drop, also creates a pre and suffix drop.
    //If I drop right before the symbol,
    // I will be able to do a forward lookup to see how much I need to drop before the symbol succeeds,
    // which could save quite a few cycles.
    def manySeparated(separator: Self[Any], elementName: String): Self[List[Result]] = {
      val zero = List.empty[Result]
      DropParser(someSeparated(separator, elementName) | succeed(zero))
    }

    def filter[Other >: Result](predicate: Other => Boolean, getMessage: Other => String) = Filter(parser, predicate, getMessage)

    def withDefault[Other >: Result](_default: Other, name: String): Self[Other] =
      WithDefault(parser, _default)

    def parseWholeInput(input: Input, mayStop: () => Boolean = () => true): ParseWholeResult[Result] = {
      parse(ParseWholeInput(parser), input, mayStop)
    }

    override def parseRoot(input: Input): ParseResult[Result] = {
      val analysis = compile(parser)
      analysis.getParse(parser)(input, newParseState(input))
    }

    def withRange[Other >: Result](addRange: (Input, Input, Result) => Other): Self[Other] = {
      val withPosition = leftRight(
        PositionParser,
        WithRemainderParser(parser),
        (left: Input, resultRight: Success[Result]) => addRange(left, resultRight.remainder, resultRight.result))
      withPosition // WithDefault(withPosition, cache => parser.getDefault(cache))
    }
  }

  case class ParseWholeInput[Result](original: Self[Result])
    extends EditorParserBase[Result] with ParserWrapper[Result] {

    override def getParser(recursive: GetParse): Parse[Result] = {
      val parseOriginal = recursive(original)

      new Parse[Result] {
        override def apply(input: Input, state: ParseState) = {
          val result = parseOriginal(input, state)
          result.mapReady(parseResult => {
            val remainder = parseResult.remainder
            if (remainder.atEnd)
              parseResult
            else {
              val error = DropError(remainder, remainder.end)
              ReadyParseResult(parseResult.resultOption, remainder.end, parseResult.history.addError(error))
            }
          })
        }
      }
    }
  }

  var biggestInput = 0
  class BiggestOfTwo[+First <: Result, +Second <: Result, Result](val first: Self[First], _second: => Self[Second])
    extends EditorParserBase[Result] with ChoiceLike[Result] {

    lazy val second = _second

    override def getParser(recursive: GetParse): Parse[Result] = {
      val parseFirst = recursive(first)
      lazy val parseSecond = recursive(second)

      new Parse[Result] {
        override def apply(input: Input, state: ParseState) = {
          biggestInput = Math.max(biggestInput, input.offset)
          val firstResult = parseFirst(input, state)
          val secondResult = parseSecond(input, state)
          val merged = firstResult.merge(secondResult)
          merged
        }
      }
    }
  }

  case class Filter[Other, Result <: Other](original: Self[Result],
                                            predicate: Other => Boolean, getMessage: Other => String)
    extends EditorParserBase[Result] with ParserWrapper[Result] {


    override def getParser(recursive: GetParse): Parse[Result] = {
      val parseOriginal = recursive(original)
      (input, state) => {
        val originalResult = parseOriginal(input, state)
        originalResult.mapReady(s => {
          s.resultOption match {
            case Some(result) => if (predicate(result)) s else ReadyParseResult(None, s.remainder,
              s.history.addError(GenericError(s.remainder, getMessage(result), History.genericErrorPenalty)))
            case None => s
          }
        })
      }
    }
  }

  case class WithRemainderParser[Result](original: Self[Result])
    extends EditorParserBase[Success[Result]] with ParserWrapper[Success[Result]] {

    override def getParser(recursive: GetParse): Parse[Success[Result]] = {
      val parseOriginal = recursive(original)
      (input, state) => parseOriginal(input, state).mapReady(r =>
        ReadyParseResult(r.resultOption.map(v => Success(v, r.remainder)), r.remainder, r.history))
    }
  }

  case class WithDefault[Result](original: Self[Result], _default: Result)
    extends EditorParserBase[Result] with ParserWrapper[Result] {

    override def getParser(recursive: GetParse): Parse[Result] = {
      val parseOriginal = recursive(original)

      def apply(input: Input, state: ParseState): ParseResult[Result] = {
        val result = parseOriginal(input, state)
        result.mapReady(ready => {
          if (ready.resultOption.isEmpty || ready.remainder == input) {
            ReadyParseResult(Some(_default), ready.remainder, ready.history)
          } else
            ready
        })
      }
      apply
    }
  }

  case class DropParser[Result](original: Self[Result]) extends EditorParserBase[Result] with ParserWrapper[Result] {

    override def getParser(recursive: GetParse): Parse[Result] = {
      val parseOriginal = recursive(original)
      lazy val result = new Parse[Result] {

        override def apply(input: Input, state: ParseState): ParseResult[Result] = {
          val originalResult = parseOriginal(input, state)

          if (input.atEnd)
            return originalResult

          val droppedInput = input.drop(1)
          val dropError = DropError(input, droppedInput)
          val dropHistory = History.error(dropError)
          val withDrop = singleResult(new DelayedParseResult(dropHistory , () => {
            apply(droppedInput, state).addHistory(dropHistory)
          }))
          originalResult.merge(withDrop)
        }
      }
      result
    }

  }

  case class DropError(from: Input, to: Input) extends MyParseError {
    def this(from: Input, expectation: String) = this(from, from.drop(1))

    override def append(next: MyParseError): Option[MyParseError] = {
      next match {
        case drop: DropError if drop.from == to =>
          Some(DropError(from, drop.to))
        case _ => None
      }
    }

    override def penalty = {
      val length = to.offset - from.offset
      History.dropMaxPenalty - History.dropReduction / (History.dropLengthShift + length)
    }

    override def message = {
      val found = from.printRange(to)
      s"Skipped '$found'"
    }

    override def range = SourceRange(from.position, to.position)
  }
}
