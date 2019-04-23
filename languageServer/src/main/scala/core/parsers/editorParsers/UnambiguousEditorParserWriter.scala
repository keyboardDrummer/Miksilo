package core.parsers.editorParsers

import core.parsers.core.UnambiguousParserWriter

trait UnambiguousEditorParserWriter extends UnambiguousParserWriter with EditorParserWriter {

  override def abort = RecursionDetected

  object RecursionDetected extends OptionFailure[Nothing] with UnamEditorParseResult[Nothing] {
    override def offset: Int = -1

    override def map[NewResult](f: Nothing => NewResult): OptionFailure[NewResult] with ParseResult[NewResult] = this

    override def partialResult: Option[Nothing] = None

    override def updateRemainder(f: Input => Input) = this

    override def flatMap[NewResult](f: Success[Nothing] => UnamEditorParseResult[NewResult]) = this

    override def successful = false

    override def resultOption = None

    override def successOption = None

    override def getSuccessRemainder = None

    override def addDefault[Other >: Nothing](value: Other, force: Boolean) = this

    override def errorsRequiredForChange = Int.MaxValue

    override def errorCount = Int.MaxValue

    override def withErrorsRequiredForChange(value: Int) = if (value == errorsRequiredForChange) this else ???
  }

  trait UnamEditorParseResult[+Result] extends UnambiguousParseResult[Result] with EditorResult[Result] {
    def successOption: Option[Success[Result]]
  }

  type ParseResult[+R] = UnamEditorParseResult[R]

  override def newFailure[Result](partial: Option[Result], input: Input, errors: List[ParseError]) =
    EditorParseResult(partial, input, errors, Int.MaxValue)

  override def newSuccess[Result](result: Result, remainder: Input) = EditorParseResult(Some(result), remainder, List.empty, Int.MaxValue)

  override def newFailure[Result](input: Input, message: String) = EditorParseResult(None, input, List(ParseError(input, message)), Int.MaxValue)

  override def leftRight[Left, Right, NewResult](left: EditorParser[Left],
                                                 right: => EditorParser[Right],
                                                 combine: (Left, Right) => NewResult): EditorParser[NewResult] =
    new Sequence(left, right, combine)

  override def succeed[NR](result: NR): EditorParser[NR] = Succeed(result)

  override def choice[Result](first: EditorParser[Result], other: => EditorParser[Result], leftIsAlwaysBigger: Boolean): EditorParser[Result] =
    if (leftIsAlwaysBigger) new OrElse(first, other) else new BiggestOfTwo(first, other)

  override def map[Result, NewResult](original: Self[Result], f: Result => NewResult): Self[NewResult] = new MapParser(original, f)

  override def lazyParser[Result](inner: => EditorParser[Result]) = new EditorLazy(inner)

  override def parseWholeInput[Result](parser: EditorParser[Result], input: Input): ParseWholeResult[Result] = {

    var maxErrors = 0
    val firstResult = parser.parseRoot(input, maxErrors)
    if (firstResult == RecursionDetected)
      return ParseWholeResult(None, List(ParseError(input, "grammar was broken")))

    var parseResult = firstResult.asInstanceOf[EditorParseResult[Result]]
    while(!parseResult.remainder.atEnd) {
      maxErrors += parseResult.errorsRequiredForChange
      val newResult = parser.parseRoot(input, maxErrors).asInstanceOf[EditorParseResult[Result]]
      // Sometimes we may not get a parseResult change even though we applied errorsRequiredForChange,
      // because when we didn't parse right, we don't know how many more errors right needs
      if (newResult.errorsRequiredForChange == Int.MaxValue) {
        if (parseResult.errors.isEmpty) {
          val didNotFinishError = ParseError(parseResult.remainder, "Did not parse entire input")
          return ParseWholeResult(parseResult.resultOption, List(didNotFinishError))
        }
        return ParseWholeResult(parseResult.resultOption, parseResult.errors)
      }
      parseResult = newResult
    }
    ParseWholeResult(parseResult.resultOption, parseResult.errors)
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

      def apply(input: Input, errorAllowance: Int): ParseResult[Result] = {
        val leftAllowance = errorAllowance //(errorAllowance * 0.9).toInt /// 2 + errorAllowance % 2
        val leftResult = parseLeft(input, leftAllowance)

        leftResult match {
          case EditorParseResult(leftOption, leftRemainder, leftErrors, changesAfterMoreErrors) =>
            if (leftErrors.size <= errorAllowance) {
              val rightRemainingErrors = errorAllowance - leftErrors.size
              parseRight(leftRemainder, rightRemainingErrors) match {
                case RecursionDetected => RecursionDetected
                case EditorParseResult(rightOption, rightRemainder, rightErrors, rightChangesAfterMoreErrors) =>
                  EditorParseResult(
                    leftOption.flatMap(l => rightOption.map(r => combine(l,r))).orElse(default),
                    rightRemainder,
                    rightErrors ++ leftErrors,
                    Math.min(rightChangesAfterMoreErrors, changesAfterMoreErrors))
              }
            }
            else {
              EditorParseResult(leftOption.flatMap(l => right.default.map(r => combine(l,r))),
                leftRemainder, leftErrors, leftErrors.size - errorAllowance)
            }

          case RecursionDetected => RecursionDetected
        }
      }

      apply
    }

    override def getDefault(cache: DefaultCache): Option[Result] = for {
      leftDefault <- cache(left)
      rightDefault <- cache(right)
    } yield combine(leftDefault, rightDefault)
  }

  class OrElse[+First <: Result, +Second <: Result, Result](val first: EditorParser[First], _second: => EditorParser[Second])
    extends EditorParserBase[Result] with ChoiceLike[Result] {

    lazy val second = _second

    override def getParser(recursive: GetParse): Parse[Result] = {
      val parseFirst = recursive(first)
      val parseSecond = recursive(second)

      def apply(input: Input, errorAllowance: Int) = {
        val firstResult = parseFirst(input, errorAllowance)
        val result = if (firstResult.errorCount > errorAllowance || firstResult == RecursionDetected) {
          val secondResult = parseSecond(input, errorAllowance)
          secondResult
        } else {
          firstResult
        }
        default.fold[ParseResult[Result]](result)(d => result.addDefault[Result](d, force = false))
      }

      apply
    }


    override def getDefault(cache: DefaultCache): Option[Result] = {
      val value: Option[First] = cache(first)
      value.orElse(cache(second))
    }
  }

  class BiggestOfTwo[+First <: Result, +Second <: Result, Result](val first: EditorParser[First], _second: => EditorParser[Second])
    extends EditorParserBase[Result] with ChoiceLike[Result] {

    lazy val second = _second

    override def getParser(recursive: GetParse): Parse[Result] = {
      val parseFirst = recursive(first)
      lazy val parseSecond = recursive(second)

      def apply(input: Input, errorAllowance: Int) = {
        val firstResult = parseFirst(input, errorAllowance)
        val secondResult = parseSecond(input, errorAllowance)
        val result = {
          if (firstResult == RecursionDetected || secondResult == RecursionDetected) {
            if (firstResult == RecursionDetected)
              secondResult
            else
              firstResult
          }
          else {
            val firstOverflow = Math.max(0, firstResult.errorCount - errorAllowance)
            val secondOverflow = Math.max(0, secondResult.errorCount - errorAllowance)
            firstOverflow.compare(secondOverflow) match {
              case 1 => secondResult.withErrorsRequiredForChange(firstOverflow)
              case -1 => firstResult.withErrorsRequiredForChange(secondOverflow)
              case 0 =>
                val errorsRequiredForChange = Math.min(firstResult.errorsRequiredForChange, secondResult.errorsRequiredForChange)
                val r = if (firstResult.offset == secondResult.offset) {
                  if (firstResult.errorCount > secondResult.errorCount) secondResult else firstResult
                } else if (firstResult.offset >= secondResult.offset) firstResult else secondResult
                r.withErrorsRequiredForChange(errorsRequiredForChange)
            }
          }
        }
        default.fold[ParseResult[Result]](result)(d => result.addDefault[Result](d))
      }

      apply
    }

    override def getDefault(cache: DefaultCache): Option[Result] = {
      val value: Option[First] = cache(first)
      value.orElse(cache(second))
    }
  }

  final case class EditorParseResult[+Result](resultOption: Option[Result], remainder: Input, errors: List[ParseError], errorsRequiredForChange: Int)
    extends UnambiguousParseResult[Result] with EditorResult [Result] with UnamEditorParseResult[Result] {

    def addDefault[Other >: Result](value: Other, force: Boolean): EditorParseResult[Other] = resultOption match {
      case Some(_) if !force => this
      case _ => EditorParseResult(Some(value), remainder, errors, errorsRequiredForChange)
    }

    def updateRemainder(f: Input => Input): EditorParseResult[Result] = {
      EditorParseResult(resultOption, f(remainder), errors, errorsRequiredForChange)
    }

    override def getSuccessRemainder = successOption.map(s => s.remainder)

    override def map[NewResult](f: Result => NewResult): ParseResult[NewResult] = {

      EditorParseResult(resultOption.map(f), remainder, errors, errorsRequiredForChange)
    }

    override def flatMap[NewResult](f: Success[Result] => ParseResult[NewResult]): ParseResult[NewResult] = {
      resultOption.map(r => f(Success(r, remainder)) match {
        case RecursionDetected => RecursionDetected
        case mapResult: EditorParseResult[NewResult] =>
          EditorParseResult(mapResult.resultOption, mapResult.remainder, mapResult.errors ++ this.errors,
            Math.min(mapResult.errorsRequiredForChange, errorsRequiredForChange))
      }).getOrElse(EditorParseResult(None, remainder, errors, errorsRequiredForChange))
    }

    override def offset = remainder.offset

    override def successOption = if (errors.isEmpty) Some(Success(resultOption.get, remainder)) else None

    override def errorCount = errors.size

    override def withErrorsRequiredForChange(value: Int) = EditorParseResult(resultOption, remainder, errors, value)
  }

}
