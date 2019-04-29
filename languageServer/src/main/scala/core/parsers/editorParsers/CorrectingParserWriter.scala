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
    while(queue.isInstanceOf[SRCons[Result]]) {
      val cons = queue.asInstanceOf[SRCons[Result]]
      val parseResult = cons.head
      queue = parseResult match {
        case parseResult: ReadyParseResult[Result] =>
          bestResult = if (bestResult != null && bestResult.score >= parseResult.score) bestResult else parseResult
          if (bestResult.remainder.atEnd)
            SREmpty
          else
            cons.tail
        case delayedResult: DelayedParseResult[Result] =>
          val results = delayedResult.results
          cons.tail.merge(results)
      }
    }
    ParseWholeResult(bestResult.resultOption, bestResult.errors)
  }

  def singleResult[Result](parseResult: LazyParseResult[Result]) = new SRCons(parseResult, SREmpty)

  type ParseResult[+Result] = SortedParseResults[Result]

  override def newFailure[Result](partial: Option[Result], input: Input, errors: List[ParseError]) =
    singleResult(ReadyParseResult(partial, input, errors))

  override def newSuccess[Result](result: Result, remainder: Input) = singleResult(ReadyParseResult(Some(result), remainder, List.empty))

  override def newFailure[Result](input: Input, message: String) = singleResult(ReadyParseResult(None, input, List(ParseError(input, message))))

  override def leftRight[Left, Right, NewResult](left: EditorParser[Left],
                                                 right: => EditorParser[Right],
                                                 combine: (Left, Right) => NewResult): EditorParser[NewResult] =
    new Sequence(left, right, combine)

  override def succeed[NR](result: NR): EditorParser[NR] = Succeed(result)

  override def choice[Result](first: EditorParser[Result], other: => EditorParser[Result], leftIsAlwaysBigger: Boolean): EditorParser[Result] =
  /*if (leftIsAlwaysBigger) new OrElse(first, other) else*/ new BiggestOfTwo(first, other)

  override def map[Result, NewResult](original: Self[Result], f: Result => NewResult): Self[NewResult] = new MapParser(original, f)

  override def lazyParser[Result](inner: => EditorParser[Result]) = new EditorLazy(inner)

  sealed trait SortedParseResults[+Result] extends ParseResultLike[Result]  {
    def merge[Other >: Result](other: SortedParseResults[Other]): SortedParseResults[Other]

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

    def mapResult[NewResult](f: LazyParseResult[Result] => LazyParseResult[NewResult]): SortedParseResults[NewResult]

    def flatMapReady[NewResult](f: ReadyParseResult[Result] => SortedParseResults[NewResult]): SortedParseResults[NewResult] = {
      flatMap[NewResult] {
        case delayed: DelayedParseResult[Result] => singleResult(new DelayedParseResult(delayed.remainder, delayed.errors, () => {
          val intermediate = delayed.results
          intermediate.flatMapReady(f)
        }))
        case ready: ReadyParseResult[Result] => f(ready)
      }
    }

    def flatMap[NewResult](f: LazyParseResult[Result] => SortedParseResults[NewResult]): SortedParseResults[NewResult]

    def updateRemainder(f: Input => Input) = {
      mapReady(r => ReadyParseResult(r.resultOption, f(r.remainder), r.errors))
    }
  }

  object SREmpty extends SortedParseResults[Nothing] {
    override def merge[Other >: Nothing](other: SortedParseResults[Other]) = other

    override def mapResult[NewResult](f: LazyParseResult[Nothing] => LazyParseResult[NewResult]): SREmpty.type = this

    override def flatMap[NewResult](f: LazyParseResult[Nothing] => SortedParseResults[NewResult]) = this

    override def map[NewResult](f: Nothing => NewResult) = this
  }

  final class SRCons[+Result](val head: LazyParseResult[Result], _tail: => SortedParseResults[Result]) extends SortedParseResults[Result] {

    def getTail = tail
    lazy val tail = _tail


//    // Detect incorrect ordering.
//    def results: List[LazyParseResult[Result]] = head :: (tail match {
//      case SREmpty => List.empty
//      case cons: SRCons[Result] => cons.results
//    })
//
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

    override def mapResult[NewResult](f: LazyParseResult[Result] => LazyParseResult[NewResult]) = {
      flatMap(r => singleResult(f(r)))
    }

    def flatMap[NewResult](f: LazyParseResult[Result] => SortedParseResults[NewResult]): SortedParseResults[NewResult] = {
      f(head) match {
        case SREmpty => tail.flatMap(f)
        case cons: SRCons[NewResult] => // Try not to evaluate tail, but if head's score gets worse, we have to otherwise the sorting may be incorrect.
          if (cons.head.score >= head.score)
            new SRCons[NewResult](cons.head, cons.tail.merge(tail.flatMap(f)))
          else
            cons.merge(tail.flatMap(f))
      }
    }

    override def map[NewResult](f: Result => NewResult): SRCons[NewResult] = {
      new SRCons(head.map(f), tail.map(f))
    }

    override def merge[Other >: Result](other: SortedParseResults[Other]): SortedParseResults[Other] = {
      other match {
        case SREmpty => this
        case other: SRCons[Other] => if (head.score >= other.head.score) {
          new SRCons(head, tail.merge(other))
        } else
          new SRCons(other.head, this.merge(other.tail))
      }
    }
  }

  trait LazyParseResult[+Result] {

    val score: Double =  {
      val result =
        // -errorSize // gives us the most correct result, but can be very slow
        remainder.offsetScore - 5 * errorSize // gets us to the end the fastest. the 5 is because sometimes a single incorrect insertion can lead to some offset gain.
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

          leftResults.flatMapReady[Result]((leftResult: ReadyParseResult[Left]) => {

            def mapRightResult(rightResult: ReadyParseResult[Right]): ReadyParseResult[Result] = ReadyParseResult(
              leftResult.resultOption.flatMap(l => rightResult.resultOption.map(r => combine(l, r))),
              rightResult.remainder,
              rightResult.errors)

            lazy val next = parseRight(leftResult.remainder, state).mapWithErrors[Result](mapRightResult, leftResult.errors)

            if (leftResult.errors.nonEmpty)
              singleResult(new DelayedParseResult(leftResult.remainder, leftResult.errors, () => next))
            else
              next
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
      WithDefault[Other](parser, cache => Some(_default))

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
      WithDefault(withPosition, cache => parser.getDefault(cache))
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

  case class WithDefault[Result](original: Self[Result], _getDefault: DefaultCache => Option[Result])
    extends EditorParserBase[Result] with ParserWrapper[Result] {

    override def getParser(recursive: GetParse): Parse[Result] = {
      val parseOriginal = recursive(original)

      new Parse[Result] {
        override def apply(input: Input, state: ParseState) = {
          val result = parseOriginal(input, state)
          result.mapReady(parseResult => {
            val newResultOption =
              if (parseResult.remainder.offset == input.offset)
                default.orElse(parseResult.resultOption)
              else
                parseResult.resultOption.orElse(default)
            ReadyParseResult(newResultOption, parseResult.remainder, parseResult.errors)
          })
        }
      }
    }

    override def getDefault(cache: DefaultCache): Option[Result] =
      _getDefault(cache)
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
