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

    override def addErrors(errors: List[ParseError]) = this

    override def errors = List.empty
  }

  trait UnamEditorParseResult[+Result] extends UnambiguousParseResult[Result] with EditorResult[Result] {
    def successOption: Option[Success[Result]]
    def addErrors(errors: List[ParseError]): ParseResult[Result]
  }

  type ParseResult[+R] = UnamEditorParseResult[R]

  override def newFailure[Result](partial: Option[Result], input: Input, errors: List[ParseError]) =
    EditorParseResult(partial, input, errors)

  override def newSuccess[Result](result: Result, remainder: Input) = EditorParseResult(Some(result), remainder, List.empty)

  override def newFailure[Result](input: Input, message: String) = EditorParseResult(None, input, List(ParseError(input, message)))

  override def leftRight[Left, Right, NewResult](left: EditorParser[Left],
                                                 right: => EditorParser[Right],
                                                 combine: (Left, Right) => NewResult): EditorParser[NewResult] =
    new Sequence(left, right, combine)

  override def succeed[NR](result: NR): EditorParser[NR] = Succeed(result)

  override def choice[Result](first: EditorParser[Result], other: => EditorParser[Result], leftIsAlwaysBigger: Boolean): EditorParser[Result] =
    if (leftIsAlwaysBigger) new OrElse(first, other) else new BiggestOfTwo(first, other)

  override def map[Result, NewResult](original: Self[Result], f: Result => NewResult): Self[NewResult] = new MapParser(original, f)

  override def lazyParser[Result](inner: => EditorParser[Result]) = new EditorLazy(inner)

  override def parseWholeInput[Result](parser: EditorParser[Result], input: Input): EditorParseResult[Result] = {

    var maxErrors = 0
    val firstResult = parser.parseRoot(input, maxErrors)
    if (firstResult == RecursionDetected)
      return EditorParseResult(None, input, List(ParseError(input, "grammar was broken")))

    var parseResult = firstResult.asInstanceOf[EditorParseResult[Result]]
    var didNotProgress = 0
    while(!parseResult.remainder.atEnd) {
      //Increment maxErrors
      maxErrors += Math.max(parseResult.errors.size, maxErrors) + 1
      val newResult = parser.parseRoot(input, maxErrors).asInstanceOf[EditorParseResult[Result]]
      if (newResult == parseResult) {
        didNotProgress += 1
      } else {
        didNotProgress = 0
      }
      if (didNotProgress > 0) {
        if (parseResult.errors.isEmpty) {
          val didNotFinishError = ParseError(parseResult.remainder, "Did not parse entire input")
          return parseResult.addErrors(List(didNotFinishError))
        }
        return parseResult
      }
      parseResult = newResult
    }
    parseResult
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
        val leftAllowance = errorAllowance / 2 + errorAllowance % 2
        val leftResult = parseLeft(input, leftAllowance)

        def noRightDefault = leftResult match {
          case failure: EditorParseResult[Left] => EditorParseResult(None, failure.remainder, failure.errors)
          case RecursionDetected => RecursionDetected
        }
        def leftWithRightDefault = right.default.map(rightDefault => leftResult.map(l => combine(l, rightDefault))).getOrElse(noRightDefault)

        leftResult match {
          case leftEditorResult@EditorParseResult(Some(leftValue),_, leftErrors)
            if leftEditorResult.errors.size <= errorAllowance =>
            val rightRemainingErrors = errorAllowance - leftEditorResult.errors.size
            val rightResult = parseRight(leftEditorResult.remainder, rightRemainingErrors)
//            if (rightResult.errors.size > rightRemainingErrors) {
//              leftWithRightDefault
//            }
//            else
            //{
              rightResult.map(r => combine(leftValue, r)).addErrors(leftErrors)
            //}

          case _ => leftWithRightDefault
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
        val result = if (firstResult.errors.size > errorAllowance || firstResult == RecursionDetected) {
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
            val firstOverflow = Math.max(0, firstResult.errors.size - errorAllowance)
            val secondOverflow = Math.max(0, secondResult.errors.size - errorAllowance)
            firstOverflow.compare(secondOverflow) match {
              case 1 => secondResult
              case -1 => firstResult
              case 0 => if (firstResult.offset > secondResult.offset) firstResult else secondResult
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

  final case class EditorParseResult[+Result](resultOption: Option[Result], remainder: Input, errors: List[ParseError])
    extends UnambiguousParseResult[Result] with EditorResult [Result] with UnamEditorParseResult[Result] {

    def addDefault[Other >: Result](value: Other, force: Boolean): EditorParseResult[Other] = resultOption match {
      case Some(_) if !force => this
      case _ => EditorParseResult(Some(value), remainder, errors)
    }

    def updateRemainder(f: Input => Input): EditorParseResult[Result] = {
      EditorParseResult(resultOption, f(remainder), errors)
    }

    override def getSuccessRemainder = successOption.map(s => s.remainder)

    override def map[NewResult](f: Result => NewResult): ParseResult[NewResult] = {

      EditorParseResult(resultOption.map(f), remainder, errors)
    }

    override def flatMap[NewResult](f: Success[Result] => ParseResult[NewResult]): ParseResult[NewResult] = {
      resultOption.map(r => f(Success(r, remainder))).getOrElse(EditorParseResult(None, remainder, errors))
    }

    override def offset = remainder.offset

    override def successOption = if (errors.isEmpty) Some(Success(resultOption.get, remainder)) else None

    override def addErrors(errors: List[ParseError]) = {
      val offset = errors.headOption.map(h => h.location.offset).getOrElse(-1)
      EditorParseResult(resultOption, remainder, this.errors.filter(e => e.location.offset > offset) ++ errors)
    }
  }

}
