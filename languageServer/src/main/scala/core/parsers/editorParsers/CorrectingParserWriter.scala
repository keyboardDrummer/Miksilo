package core.parsers.editorParsers

import core.parsers.core.OptimizingParserWriter

trait CorrectingParserWriter extends OptimizingParserWriter {

  def findBestParseResult[Result](parser: Parser[Result], input: Input, mayStop: () => Boolean): SingleParseResult[Result] = {

    val noResultFound = ReadyParseResult(None, input, History.error(FatalError(input, "Grammar is always recursive")))
    var bestResult: ReadyParseResult[Result] = noResultFound

    var resultsSeen = Set.empty[ReadyParseResult[Result]]
    var queue = parser(input, newParseState(input))
    while(queue.nonEmpty) {
      val (parseResult, tail) = queue.pop()

      queue = parseResult match {
        case parseResult: ReadyParseResult[Result] =>
          if (resultsSeen.contains(parseResult)) {
            System.out.append("Your grammar produces duplicates")
          }
          resultsSeen += parseResult

          bestResult = if (bestResult.score >= parseResult.score) bestResult else parseResult
          tail match {
            case tailCons: SRCons[Result] =>
              if (bestResult.originalScore > tailCons.head.score && mayStop())
                SREmpty
              else
                tail
            case _ => SREmpty
          }
        case delayedResult: DelayedParseResult[Result] =>
          val results = delayedResult.results
          tail.merge(results)
      }
    }
    SingleParseResult(bestResult.resultOption, bestResult.history.errors.toList)
  }

  def singleResult[Result](parseResult: LazyParseResult[Result]) =
    new SRCons(parseResult,0, SREmpty)

  type ParseResult[+Result] = SortedParseResults[Result]

  def newFailure[Result](error: MyParseError): SRCons[Result] =
    singleResult(ReadyParseResult(None, error.from, History.error(error)))

  override def leftRight[Left, Right, NewResult](left: Self[Left],
                                                 right: => Self[Right],
                                                 combine: (Left, Right) => NewResult): Self[NewResult] =
    new Sequence(left, right, combine)

  override def choice[Result](first: Self[Result], other: => Self[Result], firstIsLonger: Boolean = false): Self[Result] =
    if (firstIsLonger) new FirstIsLonger(first, other) else new Choice(first, other)

  override def map[Result, NewResult](original: Self[Result], f: Result => NewResult): Self[NewResult] = new MapParser(original, f)

  case class RecursionsList[SeedResult, +Result](recursions: List[RecursiveParseResult[SeedResult, Result]], rest: SortedParseResults[Result])

  sealed trait SortedParseResults[+Result] extends ParseResultLike[Result]  {
    def nonEmpty: Boolean
    def pop(): (LazyParseResult[Result], SortedParseResults[Result])
    def toList: List[LazyParseResult[Result]]
    def tailDepth: Int
    def merge[Other >: Result](other: SortedParseResults[Other], depth: Int = 0): SortedParseResults[Other]
    def mapResult[NewResult](f: LazyParseResult[Result] => LazyParseResult[NewResult], uniform: Boolean): SortedParseResults[NewResult]
    def flatMap[NewResult](f: LazyParseResult[Result] => SortedParseResults[NewResult], uniform: Boolean): SortedParseResults[NewResult]

    def recursionsFor[SeedResult](parse: Parser[SeedResult]): RecursionsList[SeedResult, Result]

    def addHistory(errors: MyHistory): SortedParseResults[Result] = {
      mapWithHistory(x => x, errors)
    }

    def mapWithHistory[NewResult](f: ReadyParseResult[Result] => ReadyParseResult[NewResult],
                                  oldHistory: MyHistory): SortedParseResults[NewResult] = {
      flatMap(l => l.mapWithHistory(f, oldHistory), uniform = !oldHistory.canMerge)
    }

    def mapReady[NewResult](f: ReadyParseResult[Result] => ReadyParseResult[NewResult], uniform: Boolean): SortedParseResults[NewResult] = {
      mapResult(l => l.mapReady(f, uniform), uniform)
    }

    def flatMapReady[NewResult](f: ReadyParseResult[Result] => SortedParseResults[NewResult], uniform: Boolean): SortedParseResults[NewResult] = {
      flatMap[NewResult](l => l.flatMapReady(f, uniform), uniform)
    }

    def updateRemainder(f: Input => Input) = {
      mapReady(r => ReadyParseResult(r.resultOption, f(r.remainder), r.history), uniform = true)
    }
  }

  object SREmpty extends SortedParseResults[Nothing] {
    override def merge[Other >: Nothing](other: SortedParseResults[Other], depth: Int) = other

    override def mapResult[NewResult](f: LazyParseResult[Nothing] => LazyParseResult[NewResult], uniform: Boolean): SREmpty.type = this

    override def flatMap[NewResult](f: LazyParseResult[Nothing] => SortedParseResults[NewResult], uniform: Boolean) = this

    override def map[NewResult](f: Nothing => NewResult) = this

    override def tailDepth = 0

    override def toList = List.empty

    override def recursionsFor[SeedResult](parse: Parser[SeedResult]): RecursionsList[SeedResult, Nothing] = RecursionsList(List.empty, this)

    override def nonEmpty = false

    override def pop() = throw new Exception("Can't pop empty results")
  }

  final class SRCons[+Result](val head: LazyParseResult[Result],
                              var tailDepth: Int,
                              _tail: => SortedParseResults[Result]) extends SortedParseResults[Result] {

    // Used for debugging
    def toList: List[LazyParseResult[Result]] = head :: tail.toList

    def getTail = tail
    lazy val tail = _tail

    if (tailDepth == 50) {
      tail
      tailDepth = 0
    }

    override def mapResult[NewResult](f: LazyParseResult[Result] => LazyParseResult[NewResult], uniform: Boolean): SortedParseResults[NewResult] = {
      flatMap(r => singleResult(f(r)), uniform)
    }

    def flatMap[NewResult](f: LazyParseResult[Result] => SortedParseResults[NewResult], uniform: Boolean): SortedParseResults[NewResult] = {
      f(head) match {
        case SREmpty => tail.flatMap(f, uniform)
        case cons: SRCons[NewResult] =>

          if (!uniform && head.score != cons.head.score)
            cons.merge(tail.flatMap(f, uniform))
          else
          {
            new SRCons(
              cons.head,
              1 + Math.max(this.tailDepth, cons.tailDepth),
              cons.tail.merge(tail.flatMap(f, uniform)))
          }
      }
    }

    override def map[NewResult](f: Result => NewResult): SRCons[NewResult] = {
      new SRCons(head.map(f), tailDepth + 1, tail.map(f))
    }

    override def merge[Other >: Result](other: SortedParseResults[Other], mergeDepth: Int): SortedParseResults[Other] = {
      if (mergeDepth > 300) // Should be 200, since 100 is not enough to let CorrectionJsonTest.realLifeExample2 pass
        return SREmpty

      other match {
        case SREmpty => this
        case other: SRCons[Other] =>
          if (head.score >= other.head.score) {
            new SRCons(head,1 + tailDepth, tail.merge(other, mergeDepth + 1))
          } else
            new SRCons(other.head,1 + other.tailDepth, this.merge(other.tail, mergeDepth + 1))
      }
    }

    override def recursionsFor[SeedResult](parse: Parser[SeedResult]): RecursionsList[SeedResult, Result] = head match {
      case recursive: RecursiveParseResult[_, Result] =>
        val tailResult = tail.recursionsFor(parse)
        if (recursive.parser == parse)
          RecursionsList(recursive.asInstanceOf[RecursiveParseResult[SeedResult, Result]] :: tailResult.recursions, tailResult.rest)
        else
          RecursionsList(tailResult.recursions, new SRCons[Result](recursive, 0, tailResult.rest))
      case _ =>
        RecursionsList(List.empty, this)
    }

    override def nonEmpty = true

    override def pop(): (LazyParseResult[Result], SortedParseResults[Result]) = (head, tail)
  }

  trait LazyParseResult[+Result] {
    def flatMapReady[NewResult](f: ReadyParseResult[Result] => SortedParseResults[NewResult], uniform: Boolean): SortedParseResults[NewResult]

    def mapReady[NewResult](f: ReadyParseResult[Result] => ReadyParseResult[NewResult], uniform: Boolean): LazyParseResult[NewResult]

    val score: Double = (if (history.flawed) 0 else 10000) + history.score

    def history: MyHistory
    def map[NewResult](f: Result => NewResult): LazyParseResult[NewResult]

    def mapWithHistory[NewResult](f: ReadyParseResult[Result] => ReadyParseResult[NewResult],
                       oldHistory: MyHistory): SortedParseResults[NewResult]
  }

  case class RecursiveParseResult[SeedResult, +Result](input: Input,
                                                       parser: Parser[SeedResult],
                                                       get: ParseResult[SeedResult] => ParseResult[Result])
    extends LazyParseResult[Result] {

    def history = History.empty[Input]
    override val score = 1000000 + history.score

    override def toString = "Recursive: " + parser.debugName

    override def map[NewResult](f: Result => NewResult) = {
      RecursiveParseResult[SeedResult, NewResult](input, parser, r => get(r).map(f))
    }

    override def flatMapReady[NewResult](f: ReadyParseResult[Result] => SortedParseResults[NewResult], uniform: Boolean) = {
      singleResult(RecursiveParseResult[SeedResult, NewResult](input, parser, r => get(r).flatMapReady(f, uniform)))
    }

    override def mapReady[NewResult](f: ReadyParseResult[Result] => ReadyParseResult[NewResult], uniform: Boolean) =
      RecursiveParseResult[SeedResult, NewResult](input, parser, r => get(r).mapReady(f, uniform))

    override def mapWithHistory[NewResult](f: ReadyParseResult[Result] => ReadyParseResult[NewResult], oldHistory: MyHistory) =
      if (oldHistory.flawed) {
        SREmpty
      }
      else
        singleResult(RecursiveParseResult[SeedResult, NewResult](input, parser,
          r => get(r).mapWithHistory(f, oldHistory)))
  }

  class DelayedParseResult[Result](val history: MyHistory, _getResults: () => SortedParseResults[Result])
    extends LazyParseResult[Result] {

    override def toString = score + " delayed: " + history

    override def map[NewResult](f: Result => NewResult): DelayedParseResult[NewResult] = {
      new DelayedParseResult(history, () => results.map(f))
    }

    lazy val results: SortedParseResults[Result] = _getResults()

    override def mapWithHistory[NewResult](f: ReadyParseResult[Result] => ReadyParseResult[NewResult], oldHistory: MyHistory) =
      singleResult(new DelayedParseResult(this.history ++ oldHistory, () => {
        val intermediate = this.results
        intermediate.mapWithHistory(f, oldHistory)
    }))

    override def mapReady[NewResult](f: ReadyParseResult[Result] => ReadyParseResult[NewResult], uniform: Boolean): DelayedParseResult[NewResult] =
      new DelayedParseResult(this.history, () => {
        val intermediate = this.results
        intermediate.mapReady(f, uniform)
      })

    override def flatMapReady[NewResult](f: ReadyParseResult[Result] => SortedParseResults[NewResult], uniform: Boolean) =
      singleResult(new DelayedParseResult(this.history, () => {
        val intermediate = this.results
        intermediate.flatMapReady(f, uniform)
      }))
  }

  case class ReadyParseResult[+Result](resultOption: Option[Result], remainder: Input, history: MyHistory)
    extends LazyParseResult[Result] {

    val originalScore = (if (history.flawed) 0 else 10000) + history.score
    override val score = 10000 + originalScore

    override def map[NewResult](f: Result => NewResult): ReadyParseResult[NewResult] = {
      ReadyParseResult(resultOption.map(f), remainder, history)
    }

    override def mapWithHistory[NewResult](f: ReadyParseResult[Result] => ReadyParseResult[NewResult], oldHistory: MyHistory) = {
      val newReady = f(this)
      singleResult(ReadyParseResult(newReady.resultOption, newReady.remainder, newReady.history ++ oldHistory))
    }

    override def mapReady[NewResult](f: ReadyParseResult[Result] => ReadyParseResult[NewResult], uniform: Boolean): ReadyParseResult[NewResult] = f(this)

    override def flatMapReady[NewResult](f: ReadyParseResult[Result] => SortedParseResults[NewResult], uniform: Boolean) = f(this)
  }

  class Sequence[+Left, +Right, Result](val left: Self[Left],
                                         _right: => Self[Right],
                                         combine: (Left, Right) => Result)
    extends ParserBuilderBase[Result] with SequenceLike[Result] {

    lazy val right: Self[Right] = _right

    override def getParser(recursive: GetParser): Parser[Result] = {
      val parseLeft = recursive(left)
      lazy val parseRight = recursive(right)

      new Parser[Result] {
        override def apply(input: Input, state: ParseState) = {
          val leftResults = parseLeft(input, state)

          val delayedLeftResults: SortedParseResults[Left] = leftResults.mapResult({
            case ready: ReadyParseResult[Left] =>
              if (ready.history.flawed)
                new DelayedParseResult[Left](ready.history, () => singleResult(ready))
              else
                ready
            case lazyResult => lazyResult
          }, false) // TODO set to true?

          def rightFromLeftReady(leftReady: ReadyParseResult[Left]): SortedParseResults[Result] = {
            def mapRightResult(rightResult: ReadyParseResult[Right]): ReadyParseResult[Result] = ReadyParseResult(
              leftReady.resultOption.flatMap(l => rightResult.resultOption.map(r => combine(l, r))),
              rightResult.remainder,
              rightResult.history)

            val rightResult = parseRight(leftReady.remainder, state)
            rightResult.mapWithHistory[Result](mapRightResult, leftReady.history)
          }
          delayedLeftResults.flatMapReady(rightFromLeftReady, uniform = false)
        }
      }
    }
  }

  class FirstIsLonger[+First <: Result, +Second <: Result, Result](val first: Self[First], _second: => Self[Second])
    extends ParserBuilderBase[Result] with ChoiceLike[Result] {

    lazy val second = _second

    override def getParser(recursive: GetParser): Parser[Result] = {
      val parseFirst = recursive(first)
      lazy val parseSecond = recursive(second)

      new Parser[Result] {
        override def apply(input: Input, state: ParseState) = {
          val firstResult = parseFirst(input, state)
          val secondResult = parseSecond(input, state)
          firstResult match {
            case cons: SRCons[Result] if !cons.head.history.flawed => firstResult
            case _ =>
              firstResult.merge(secondResult)
          }
        }
      }
    }
  }

  class Choice[+First <: Result, +Second <: Result, Result](val first: Self[First], _second: => Self[Second])
    extends ParserBuilderBase[Result] with ChoiceLike[Result] {

    lazy val second = _second

    override def getParser(recursive: GetParser): Parser[Result] = {
      val parseFirst = recursive(first)
      lazy val parseSecond = recursive(second)

      new Parser[Result] {
        override def apply(input: Input, state: ParseState) = {
          val firstResult = parseFirst(input, state)
          val secondResult = parseSecond(input, state)
          val merged = firstResult.merge(secondResult)
          merged
        }
      }
    }
  }

  case class WithRangeParser[Result, NewResult](original: Self[Result], addRange: (Input, Input, Result) => NewResult)
    extends ParserBuilderBase[NewResult] with ParserWrapper[NewResult] {

    override def getParser(recursive: GetParser): Parser[NewResult] = {
      val parseOriginal = recursive(original)
      (input, state) => {
        parseOriginal(input, state).mapReady(ready => {
          val newValue = ready.resultOption.map(v => addRange(input, ready.remainder, v))
          ReadyParseResult(newValue, ready.remainder, ready.history)
        }, uniform = true)
      }
    }
  }

  override def succeed[Result](result: Result): Self[Result] = Succeed(result)

  case class SingleParseResult[Result](resultOption: Option[Result], errors: List[MyParseError]) {
    def successful = errors.isEmpty
    def get: Result = resultOption.get
  }

  def newSuccess[Result](result: Result, remainder: Input, score: Double): SRCons[Result] =
    singleResult(ReadyParseResult(Some(result), remainder, SpotlessHistory().addSuccess(remainder, remainder, result, score)))

  case class Succeed[Result](value: Result) extends ParserBuilderBase[Result] with LeafParser[Result] {

    override def getParser(recursive: GetParser): Parser[Result] = {
      (input: Input, _) => newSuccess(value, input, 0)
    }

    override def getMustConsume(cache: ConsumeCache) = false
  }

  class MapParser[Result, NewResult](val original: Self[Result], f: Result => NewResult)
    extends ParserBuilderBase[NewResult] with ParserWrapper[NewResult] {

    override def getParser(recursive: GetParser): Parser[NewResult] = {
      val parseOriginal = recursive(original)

      new Parser[NewResult] {
        override def apply(input: Input, state: ParseState): ParseResult[NewResult] = parseOriginal(input, state).map(f)
      }
    }
  }

  def newFailure[Result](partial: Option[Result], input: Input, errors: MyHistory) =
    singleResult(ReadyParseResult(partial, input, errors))

  type MyParseError = ParseError[Input]
  type MyHistory = History[Input]

  case class FatalError(location: Input, message: String, penalty: Double = History.failPenalty) extends MyParseError {
    override def append(other: MyParseError): Option[MyParseError] = None

    override def from = location

    override def to = from
  }
}
