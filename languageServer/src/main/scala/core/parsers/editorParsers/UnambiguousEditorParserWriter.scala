package core.parsers.editorParsers

import core.parsers.core.UnambiguousParserWriter

import scala.collection.mutable

trait UnambiguousEditorParserWriter extends UnambiguousParserWriter with EditorParserWriter {

  override def abort = Results[Nothing](List.empty)

  case class Results[+Result](options: List[ParseContinuable[Result]]) extends EditorResult[Result] {
    def this(single: ParseContinuable[Result]) = this(List(single))

    def updateRemainder(f: Input => Input) = {
      mapResult(r => EditorParseResult(r.resultOption, f(r.remainder), r.errors))
    }

    def mapResult[NewResult](f: EditorParseResult[Result] => EditorParseResult[NewResult]): Results[NewResult] = {
      Results[NewResult](options.map {
        case continuation: ParseContinuation[Result] => continuation.mapResult(f)
        case result: EditorParseResult[Result] => f(result)
      })
    }

    def flatMap[NewResult](f: EditorParseResult[Result] => Results[NewResult]): Results[NewResult] = {
      Results(options.flatMap({
        case continuation: ParseContinuation[Result] => List(continuation.flatMap(f))
        case result: EditorParseResult[Result] => f(result).options
      }))
    }

    override def map[NewResult](f: Result => NewResult) = {
      Results(options.map(o => o.map(f)))
    }
  }

  trait ParseContinuable[+Result] {
    def map[NewResult](f: Result => NewResult): ParseContinuable[NewResult]
  }

  case class ParseContinuation[+Result](remainder: Input, errors: List[ParseError], continuation: () => Results[Result])
    extends ParseContinuable[Result] {

    def flatMap[NewResult](f: EditorParseResult[Result] => Results[NewResult]): ParseContinuation[NewResult] = {
      ParseContinuation(remainder, errors, () => {
        val intermediate = continuation()
        intermediate.flatMap(f)
      })
    }

    def mapResult[NewResult](f: EditorParseResult[Result] => EditorParseResult[NewResult]): ParseContinuation[NewResult] = {
      ParseContinuation(remainder, errors, () => {
        val intermediate = continuation()
        intermediate.mapResult(f)
      })
    }

    override def map[NewResult](f: Result => NewResult): ParseContinuation[NewResult] = {
      ParseContinuation(remainder, errors, () => continuation().map(f))
    }
  }

  type ParseResult[+Result] = Results[Result]

  override def newFailure[Result](partial: Option[Result], input: Input, errors: List[ParseError]) =
    new Results[Result](EditorParseResult(partial, input, errors))

  override def newSuccess[Result](result: Result, remainder: Input) = new Results(EditorParseResult(Some(result), remainder, List.empty))

  override def newFailure[Result](input: Input, message: String) = new Results(EditorParseResult(None, input, List(ParseError(input, message))))

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

    val ordering = Ordering.by[ParseContinuation[Any], (Int, Int)](c => (-c.errors.size, c.remainder.offset))
    val queue = new mutable.PriorityQueue[ParseContinuation[Any]]()(ordering)
    queue.enqueue(ParseContinuation(input, List.empty, () => parser.parseRoot(input)))

    while(queue.nonEmpty) {
      val element = queue.dequeue()
      val options = element.continuation().options
      for(option <- options) {
        option match {
          case EditorParseResult(resultOption, resultRemainder, resultErrors) if resultRemainder.atEnd =>
            return ParseWholeResult(resultOption.asInstanceOf[Option[Result]], resultErrors)
          case continuation: ParseContinuation[Any] => queue.enqueue(continuation)
          case _ =>
        }
      }
    }
    val didNotFinishError = ParseError(input, "Did not parse entire input")
    ParseWholeResult(None, List(didNotFinishError))
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

        val waitAfterErrorLeftResults = Results(leftResults.options.map {
          case continuation: ParseContinuation[Left] => continuation
          case result: EditorParseResult[Left] => if (result.errors.isEmpty) result else
            new ParseContinuation[Left](result.remainder, result.errors, () => new Results(result))
        })
        waitAfterErrorLeftResults.flatMap[Result]({ case EditorParseResult(leftOption, leftRemainder, leftErrors) =>
          parseRight(leftRemainder).mapResult[Result]({ case EditorParseResult(rightOption, rightRemainder, rightErrors) =>
            EditorParseResult(
              leftOption.flatMap(l => rightOption.map(r => combine(l,r))),
              rightRemainder,
              rightErrors ++ leftErrors)
          })
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
        Results(firstResult.options ++ secondResult.options)
        //default.fold[ParseResult[Result]](result)(d => result.addDefault[Result](d))
      }

      apply
    }

    override def getDefault(cache: DefaultCache): Option[Result] = {
      val value: Option[First] = cache(first)
      value.orElse(cache(second))
    }
  }

  case class EditorParseResult[+Result](resultOption: Option[Result], remainder: Input, errors: List[ParseError])
    extends ParseContinuable[Result] {

//    def addDefault[Other >: Result](value: Other, force: Boolean): EditorParseResult[Other] = resultOption match {
//      case Some(_) if !force => this
//      case _ => EditorParseResult(Some(value), remainder, errors)
//    }
//
//    def updateRemainder(f: Input => Input): EditorParseResult[Result] = {
//      EditorParseResult(resultOption, f(remainder), errors)
//    }
    // override def getSuccessRemainder = successOption.map(s => s.remainder)

    override def map[NewResult](f: Result => NewResult): EditorParseResult[NewResult] = {
      EditorParseResult(resultOption.map(f), remainder, errors)
    }

//    override def flatMap[NewResult](f: Success[Result] => ParseResult[NewResult]): ParseResult[NewResult] = {
//      resultOption.map(r => f(Success(r, remainder)) match {
//        case RecursionDetected => RecursionDetected
//        case mapResult: EditorParseResult[NewResult] =>
//          EditorParseResult(mapResult.resultOption, mapResult.remainder, mapResult.errors ++ this.errors,
//            Math.min(mapResult.errorsRequiredForChange, errorsRequiredForChange))
//      }).getOrElse(EditorParseResult(None, remainder, errors, errorsRequiredForChange))
//    }

//    override def offset = remainder.offset
//
//    override def successOption = if (errors.isEmpty) Some(Success(resultOption.get, remainder)) else None
//
//    override def errorCount = errors.size
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
