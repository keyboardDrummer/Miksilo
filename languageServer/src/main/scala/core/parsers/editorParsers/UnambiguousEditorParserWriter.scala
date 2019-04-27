package core.parsers.editorParsers

import core.parsers.core.UnambiguousParserWriter

import scala.annotation.tailrec

trait UnambiguousEditorParserWriter extends UnambiguousParserWriter with EditorParserWriter {

  override def parseWholeInput[Result](parser: EditorParser[Result], input: Input): ParseWholeResult[Result] = {

    @tailrec
    def emptyQueue(queue: SortedParseResults[Result]): ParseWholeResult[Result] = {
      queue match {
        case SREmpty =>
          val didNotFinishError = ParseError(input, "Did not parse entire input")
          ParseWholeResult(None, List(didNotFinishError))
        case cons: SRCons[Result] =>
          var newQueue = cons.tail
          val parseResult = cons.head
          parseResult match {
            case ReadyParseResult(resultOption, resultRemainder, resultErrors) if resultRemainder.atEnd =>
              return ParseWholeResult(resultOption.asInstanceOf[Option[Result]], resultErrors)
            case delayedResult: DelayedParseResult[Any] =>
              newQueue = newQueue.merge(delayedResult.continuation())
            case _ =>
          }
          emptyQueue(newQueue)
      }
    }

    emptyQueue(parser.parseRoot(input))
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


  override def newParseState(parser: EditorParser[_]) = new LeftRecursionDetectorState()

  override def abort = ??? //SortedResults[Nothing](List.empty)

  trait SortedParseResults[+Result] extends EditorResult[Result]  {
    def merge[Other >: Result](other: SortedParseResults[Other]): SortedParseResults[Other]

    def addErrors(errors: List[ParseError]): SortedParseResults[Result] = {
      mapResult({
        case delayed: DelayedParseResult[Result] => DelayedParseResult(delayed.remainder, delayed.errors ++ errors, () => {
          val intermediate = delayed.continuation()
          intermediate.addErrors(errors)
        })
        case ready: ReadyParseResult[Result] =>
          ReadyParseResult(ready.resultOption, ready.remainder, ready.errors ++ errors)
      })
    }

    def mapWithErrors[NewResult](f: ReadyParseResult[Result] => ReadyParseResult[NewResult],
                                 oldErrors: List[ParseError]): SortedParseResults[NewResult] = {
      mapResult({
        case delayed: DelayedParseResult[Result] => DelayedParseResult(delayed.remainder, delayed.errors ++ oldErrors, () => {
          val intermediate = delayed.continuation()
          intermediate.mapWithErrors(f, oldErrors)
        })
        case ready: ReadyParseResult[Result] =>
          val newReady = f(ready)
          ReadyParseResult(newReady.resultOption, newReady.remainder, newReady.errors ++ oldErrors)
      })
    }

    def mapResultSimple[NewResult](f: ReadyParseResult[Result] => ReadyParseResult[NewResult]): SortedParseResults[NewResult] = {
      mapResult({
        case delayed: DelayedParseResult[Result] => DelayedParseResult(delayed.remainder, delayed.errors, () => {
            val intermediate = delayed.continuation()
            intermediate.mapResultSimple(f)
          })
        case ready: ReadyParseResult[Result] => f(ready)
      })
    }

    def mapResult[NewResult](f: LazyParseResult[Result] => LazyParseResult[NewResult]): SortedParseResults[NewResult]

    def flatMap[NewResult](f: ReadyParseResult[Result] => SortedParseResults[NewResult]): SortedParseResults[NewResult]

    def updateRemainder(f: Input => Input) = {
      mapResultSimple(r => ReadyParseResult(r.resultOption, f(r.remainder), r.errors))
    }
  }

  object SREmpty extends SortedParseResults[Nothing] {
    override def merge[Other >: Nothing](other: SortedParseResults[Other]) = other

    override def mapResult[NewResult](f: LazyParseResult[Nothing] => LazyParseResult[NewResult]): SREmpty.type = this

    override def flatMap[NewResult](f: ReadyParseResult[Nothing] => SortedParseResults[NewResult]) = this

    override def map[NewResult](f: Nothing => NewResult) = this
  }

  class SRCons[+Result](val head: LazyParseResult[Result], _tail: => SortedParseResults[Result]) extends SortedParseResults[Result] {

    val tail = _tail
    def results: List[LazyParseResult[Result]] = head :: (tail match {
      case SREmpty => List.empty
      case cons: SRCons[Result] => cons.results
    })

    val score = head.score
    for(result <- results.drop(0)) {
      if (result.score > score) {
        System.out.append("")
      }
    }

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
      new SRCons[NewResult](f(head), tail.mapResult(f))
    }

    def flatMap[NewResult](f: ReadyParseResult[Result] => SortedParseResults[NewResult]): SortedParseResults[NewResult] = {
      head.flatMap(f) match {
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

    def score: Double =
      // -errors.size // gives us the most correct result, but can be very slow
      remainder.offset - 5 * errors.size // gets us to the end the fastest. the 5 is because sometimes a single incorrect insertion can lead to some offset gain.
      // remainder.offset / (errors.size + 1) // compromise

    def errors: List[ParseError]
    def remainder: Input

    def flatMap[NewResult](f: ReadyParseResult[Result] => SortedParseResults[NewResult]): SortedParseResults[NewResult]
    def map[NewResult](f: Result => NewResult): LazyParseResult[NewResult]
  }

  case class DelayedParseResult[+Result](remainder: Input, errors: List[ParseError], continuation: () => SortedParseResults[Result])
    extends LazyParseResult[Result] {

    if (errors.isEmpty) {
      System.out.append("")
    }

    def flatMap[NewResult](f: ReadyParseResult[Result] => SortedParseResults[NewResult]): SortedParseResults[NewResult] = {
      singleResult(DelayedParseResult(remainder, errors, () => {
        val intermediate = continuation()
        intermediate.flatMap(f)
      }))
    }

    override def map[NewResult](f: Result => NewResult): DelayedParseResult[NewResult] = {
      DelayedParseResult(remainder, errors, () => continuation().map(f))
    }
  }

  case class ReadyParseResult[+Result](resultOption: Option[Result], remainder: Input, errors: List[ParseError])
    extends LazyParseResult[Result] {

    override def map[NewResult](f: Result => NewResult): ReadyParseResult[NewResult] = {
      ReadyParseResult(resultOption.map(f), remainder, errors)
    }

    override def flatMap[NewResult](f: ReadyParseResult[Result] => SortedParseResults[NewResult]) = f(this)
  }

  class Sequence[+Left, +Right, Result](val left: EditorParser[Left],
                                         _right: => EditorParser[Right],
                                         combine: (Left, Right) => Result)
    extends EditorParserBase[Result] with SequenceLike[Result] {

    lazy val right: EditorParser[Right] = _right

    override def getParser(recursive: GetParse): Parse[Result] = {
      val parseLeft = recursive(left)
      lazy val parseRight = recursive(right)

      def apply(input: Input): ParseResult[Result] = {
        val leftResults = parseLeft(input)

        leftResults.flatMap[Result]({ case ReadyParseResult(leftOption, leftRemainder, leftErrors) =>

          def mapRightResult(rightResult: ReadyParseResult[Right]): ReadyParseResult[Result] = ReadyParseResult(
            leftOption.flatMap(l => rightResult.resultOption.map(r => combine(l, r))),
            rightResult.remainder,
            rightResult.errors)
          lazy val next = parseRight(leftRemainder).mapWithErrors[Result](mapRightResult, leftErrors)

          if (leftErrors.nonEmpty)
            singleResult(DelayedParseResult(leftRemainder, leftErrors, () => next))
          else
            next
        })
      }

      apply
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
      UnambiguousEditorParserWriter.this.parseWholeInput(parser, input)
    }

    override def parseRoot(input: Input): ParseResult[Result] = {
      setDefaults(parser)
      val analysis = compile(parser)
      analysis.getParse(parser)(input)
    }

    def withRange[Other >: Result](addRange: (Input, Input, Result) => Other): EditorParser[Other] = {
      val withPosition = leftRight(
        PositionParser,
        WithRemainderParser(parser),
        (left: Input, resultRight: Success[Result]) => addRange(left, resultRight.remainder, resultRight.result))
      WithDefault(withPosition, cache => parser.getDefault(cache))
    }
  }


//  class OrElse[+First <: Result, +Second <: Result, Result](val first: EditorParser[First], _second: => EditorParser[Second])
//    extends EditorParserBase[Result] with ChoiceLike[Result] {
//
//    lazy val second = _second
//
//    override def getParser(recursive: GetParse): Parse[Result] = {
//      val parseFirst = recursive(first)
//      val parseSecond = recursive(second)
//
//      def apply(input: Input, errorAllowance: Int) = {
//        val firstResult = parseFirst(input, errorAllowance)
//        val result = if (firstResult.errorCount > errorAllowance || firstResult == RecursionDetected) {
//          val secondResult = parseSecond(input, errorAllowance)
//          secondResult
//        } else {
//          firstResult
//        }
//        default.fold[ParseResult[Result]](result)(d => result.addDefault[Result](d, force = false))
//      }
//
//      apply
//    }
//
//
//    override def getDefault(cache: DefaultCache): Option[Result] = {
//      val value: Option[First] = cache(first)
//      value.orElse(cache(second))
//    }
//  }

  class BiggestOfTwo[+First <: Result, +Second <: Result, Result](val first: EditorParser[First], _second: => EditorParser[Second])
    extends EditorParserBase[Result] with ChoiceLike[Result] {

    lazy val second = _second

    override def getParser(recursive: GetParse): Parse[Result] = {
      val parseFirst = recursive(first)
      lazy val parseSecond = recursive(second)

      def apply(input: Input) = {
        val firstResult = parseFirst(input)
        val secondResult = parseSecond(input)
        firstResult.merge(secondResult)
        //default.fold[ParseResult[Result]](result)(d => result.addDefault[Result](d))
      }

      apply
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
        def apply(input: Input): ParseResult[Result] = {
          val result = parseOriginal(input)
          result.mapResultSimple(parseResult => {
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
      input => {
        val originalResult = parseOriginal(input)
        originalResult.mapResultSimple(s => {
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
      input => parseOriginal(input).mapResultSimple(r =>
        ReadyParseResult(r.resultOption.map(v => Success(v, r.remainder)), r.remainder, r.errors))
    }

    override def getDefault(cache: DefaultCache): Option[Success[Result]] = None
  }

}
