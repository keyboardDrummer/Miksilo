package core.parsers.editorParsers

import core.parsers.core.UnambiguousParserWriter

import scala.annotation.tailrec

trait UnambiguousEditorParserWriter extends UnambiguousParserWriter with EditorParserWriter {

  override def abort = ??? //SortedResults[Nothing](List.empty)

  trait SortedParseResults[+Result] extends EditorResult[Result]  {
    def merge[Other >: Result](other: SortedParseResults[Other]): SortedParseResults[Other]

    def mapResult[NewResult](f: EditorParseResult[Result] => EditorParseResult[NewResult]): SortedParseResults[NewResult]
    def flatMap[NewResult](f: EditorParseResult[Result] => SortedParseResults[NewResult]): SortedParseResults[NewResult]

    def updateRemainder(f: Input => Input) = {
      mapResult(r => EditorParseResult(r.resultOption, f(r.remainder), r.errors))
    }
  }

  object SREmpty extends SortedParseResults[Nothing] {
    override def merge[Other >: Nothing](other: SortedParseResults[Other]) = other

    override def mapResult[NewResult](f: EditorParseResult[Nothing] => EditorParseResult[NewResult]) = this

    override def flatMap[NewResult](f: EditorParseResult[Nothing] => SortedParseResults[NewResult]) = this

    override def map[NewResult](f: Nothing => NewResult) = this
  }

  class SRCons[+Result](val head: LazyParseResult[Result], _tail: => SortedParseResults[Result]) extends SortedParseResults[Result] {

    var switch = true
    def tail = {
      if (switch) {
        switch = false
        val result = _tail
        result
      }
      else {
        ???
      }
    }

    def mapResult[NewResult](f: EditorParseResult[Result] => EditorParseResult[NewResult]): SortedParseResults[NewResult] = {
      new SRCons[NewResult](head.mapResult(f),
        tail.mapResult(f)
      )
    }

    def flatMap[NewResult](f: EditorParseResult[Result] => SortedParseResults[NewResult]): SortedParseResults[NewResult] = {
      head.flatMap(f) match {
        case SREmpty => tail.flatMap(f)
        case cons: SRCons[NewResult] => new SRCons[NewResult](cons.head, cons.tail.merge(tail.flatMap(f)))
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

    def score: Double

    def mapResult[NewResult](f: EditorParseResult[Result] => EditorParseResult[NewResult]): LazyParseResult[NewResult]
    def flatMap[NewResult](f: EditorParseResult[Result] => SortedParseResults[NewResult]): SortedParseResults[NewResult]
    def map[NewResult](f: Result => NewResult): LazyParseResult[NewResult]
  }

  case class DelayedParseResult[+Result](remainder: Input, errors: List[ParseError], continuation: () => SortedParseResults[Result])
    extends LazyParseResult[Result] {

    if (errors.toString().contains("""ParseError({"person | ,expected '{' but end of source found)""")) {
      System.out.append("")
    }

    if (errors.isEmpty) {
      System.out.append("")
    }

    def flatMap[NewResult](f: EditorParseResult[Result] => SortedParseResults[NewResult]): SortedParseResults[NewResult] = {
      singleResult(DelayedParseResult(remainder, errors, () => {
        val intermediate = continuation()
        intermediate.flatMap(f)
      }))
    }

    def mapResult[NewResult](f: EditorParseResult[Result] => EditorParseResult[NewResult]): DelayedParseResult[NewResult] = {
      DelayedParseResult(remainder, errors, () => {
        val intermediate = continuation()
        intermediate.mapResult(f)
      })
    }

    override def map[NewResult](f: Result => NewResult): DelayedParseResult[NewResult] = {
      DelayedParseResult(remainder, errors, () => continuation().map(f))
    }

    override def score = remainder.offset.toDouble / (errors.size + 1)
  }

  case class EditorParseResult[+Result](resultOption: Option[Result], remainder: Input, errors: List[ParseError])
    extends LazyParseResult[Result] {

    override def map[NewResult](f: Result => NewResult): EditorParseResult[NewResult] = {
      EditorParseResult(resultOption.map(f), remainder, errors)
    }

    override def score = remainder.offset.toDouble / (errors.size + 1)

    override def mapResult[NewResult](f: EditorParseResult[Result] => EditorParseResult[NewResult]) = f(this)

    override def flatMap[NewResult](f: EditorParseResult[Result] => SortedParseResults[NewResult]) = f(this)
  }

  def singleResult[Result](parseResult: LazyParseResult[Result]) = new SRCons(parseResult, SREmpty)

  type ParseResult[+Result] = SortedParseResults[Result]

  override def newFailure[Result](partial: Option[Result], input: Input, errors: List[ParseError]) =
    singleResult(EditorParseResult(partial, input, errors))

  override def newSuccess[Result](result: Result, remainder: Input) = singleResult(EditorParseResult(Some(result), remainder, List.empty))

  override def newFailure[Result](input: Input, message: String) = singleResult(EditorParseResult(None, input, List(ParseError(input, message))))

  override def leftRight[Left, Right, NewResult](left: EditorParser[Left],
                                                 right: => EditorParser[Right],
                                                 combine: (Left, Right) => NewResult): EditorParser[NewResult] =
    new Sequence(left, right, combine)

  override def succeed[NR](result: NR): EditorParser[NR] = Succeed(result)

  override def choice[Result](first: EditorParser[Result], other: => EditorParser[Result], leftIsAlwaysBigger: Boolean): EditorParser[Result] =
    /*if (leftIsAlwaysBigger) new OrElse(first, other) else*/ new BiggestOfTwo(first, other)

  override def map[Result, NewResult](original: Self[Result], f: Result => NewResult): Self[NewResult] = new MapParser(original, f)

  override def lazyParser[Result](inner: => EditorParser[Result]) = new EditorLazy(inner)

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
            case EditorParseResult(resultOption, resultRemainder, resultErrors) if resultRemainder.atEnd =>
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

  override def newParseState(parser: EditorParser[_]) = new LeftRecursionDetectorState()

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

        leftResults.flatMap[Result]({ case EditorParseResult(leftOption, leftRemainder, leftErrors) =>

          lazy val next = parseRight(leftRemainder).mapResult[Result]({ case EditorParseResult(rightOption, rightRemainder, rightErrors) =>
            EditorParseResult(
              leftOption.flatMap(l => rightOption.map(r => combine(l,r))),
              rightRemainder,
              rightErrors ++ leftErrors)
          })

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

      def apply(input: Input): ParseResult[Result] = {
        val result = parseOriginal(input)
        result.mapResult(parseResult => {
          val newResultOption =
            if (parseResult.remainder.offset == input.offset)
              default.orElse(parseResult.resultOption)
            else
              parseResult.resultOption.orElse(default)
          EditorParseResult(newResultOption, parseResult.remainder, parseResult.errors)
        })
      }

      apply
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
        originalResult.mapResult(s => {
          s.resultOption match {
            case Some(result) => if (predicate(result)) s else EditorParseResult(default, s.remainder,
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
      input => parseOriginal(input).mapResult(r =>
        EditorParseResult(r.resultOption.map(v => Success(v, r.remainder)), r.remainder, r.errors))
    }

    override def getDefault(cache: DefaultCache): Option[Success[Result]] = None
  }

}
