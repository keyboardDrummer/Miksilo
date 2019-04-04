package core.parsers.editorParsers

import core.parsers.core.UnambiguousParserWriter

trait UnambiguousEditorParserWriter extends UnambiguousParserWriter with EditorParserWriter {

  override def abort = EditorParseResult(None, NoFailure)

  type ParseResult[+R] = EditorParseResult[R]

  override def flatMap[Result, NewResult](left: EditorParser[Result], getRight: Result => EditorParser[NewResult]) = ???

  override def newFailure[Result](partial: Option[Result], input: Input, message: String) = EditorParseResult(None, ParseFailure(partial, input, message))

  override def newSuccess[Result](result: Result, remainder: Input) = EditorParseResult(Some(Success(result, remainder)), NoFailure)

  override def newFailure[Result](input: Input, message: String) = EditorParseResult(None, ParseFailure(None, input, message))

  override def leftRight[Left, Right, NewResult](left: EditorParser[Left],
                                                 right: => EditorParser[Right],
                                                 combine: (Left, Right) => NewResult): EditorParser[NewResult] =
    new Sequence(left, right, combine)

  override def succeed[NR](result: NR): EditorParser[NR] = Succeed(result)

  override def choice[Result](first: EditorParser[Result], other: => EditorParser[Result], leftIsAlwaysBigger: Boolean): EditorParser[Result] =
    if (leftIsAlwaysBigger) new OrElse(first, other) else new BiggestOfTwo(first, other)

  override def map[Result, NewResult](original: Self[Result], f: Result => NewResult): Self[NewResult] = new MapParser(original, f)

  override def lazyParser[Result](inner: => EditorParser[Result]) = new EditorLazy(inner)

  override def parseWholeInput[Result](parser: EditorParser[Result], input: Input) = {

    val parseResult = parser.parseFinal(input)
    parseResult.successOption match {
      case Some(success) =>
        if (success.remainder.atEnd) parseResult
        else {
          val failedSuccess = ParseFailure(Some(success.result), success.remainder, "Did not parse entire input")
          EditorParseResult(None, failedSuccess.getBiggest(parseResult.biggestFailure))
        }
      case None => parseResult
    }
  }

  override def newParseState(parser: EditorParser[_]) = new PackratParseState()

  class Sequence[+Left, +Right, Result](left: EditorParser[Left],
                                         _right: => EditorParser[Right],
                                         combine: (Left, Right) => Result) extends EditorParserBase[Result] {
    lazy val right: EditorParser[Right] = _right

    override def parseInternal(input: Input, state: ParseState): ParseResult[Result] = {
      val leftResult = left.parse(input, state)
      val leftFailure = right.default.map(rightDefault => leftResult.biggestFailure.map(l => combine(l, rightDefault))).
        getOrElse(leftResult.biggestFailure match {
          case NoFailure => NoFailure
          case failure: ParseFailure[Left] => ParseFailure(None, failure.remainder, failure.message)
        })
      leftResult.successOption match {
        case Some(leftSuccess) =>
          val rightResult = right.parse(leftSuccess.remainder, state)
          rightResult.map(r => combine(leftSuccess.result, r)).addFailure(leftFailure)

        case None =>
          EditorParseResult(None, leftFailure)
      }
    }

    override def getDefault(cache: DefaultCache): Option[Result] = for {
      leftDefault <- cache(left)
      rightDefault <- cache(right)
    } yield combine(leftDefault, rightDefault)

    override def children = List(left, right)
  }

  class OrElse[+First <: Result, +Second <: Result, Result](first: EditorParser[First], _second: => EditorParser[Second])
    extends EditorParserBase[Result] {
    lazy val second = _second

    override def parseInternal(input: Input, state: ParseState): ParseResult[Result] = {
      val firstResult = first.parse(input, state)
      val result = firstResult.successOption match {
        case Some(_) => firstResult
        case None =>
          val secondResult = second.parse(input, state)
          secondResult.addFailure(firstResult.biggestFailure)
      }
      default.fold[ParseResult[Result]](result)(d => result.addDefault[Result](d))
    }

    override def getDefault(cache: DefaultCache): Option[Result] = {
      val value: Option[First] = cache(first)
      value.orElse(cache(second))
    }

    override def children = List(first, second)
  }

  class BiggestOfTwo[+First <: Result, +Second <: Result, Result](first: EditorParser[First], _second: => EditorParser[Second])
    extends EditorParserBase[Result] {
    lazy val second = _second

    def parseInternal(input: Input, state: ParseState): ParseResult[Result] = {
      val firstResult = first.parse(input, state)
      val secondResult = second.parse(input, state)
      val result = (firstResult.successOption, secondResult.successOption) match {
        case (Some(firstSuccess), Some(secondSuccess)) =>
          if (firstSuccess.remainder.offset >= secondSuccess.remainder.offset)
            firstResult.addFailure(secondResult.biggestFailure)
          else
            secondResult.addFailure(firstResult.biggestFailure)
        case (None, Some(_)) =>
          secondResult.addFailure(firstResult.biggestFailure)
        case (Some(_), None) =>
          firstResult.addFailure(secondResult.biggestFailure)
        case (None, None) =>
          firstResult.addFailure(secondResult.biggestFailure)
        case _ => throw new Exception("can not occur")
      }
      default.fold[ParseResult[Result]](result)(d => result.addDefault[Result](d))
    }

    override def getDefault(cache: DefaultCache): Option[Result] = {
      val value: Option[First] = cache(first)
      value.orElse(cache(second))
    }

    override def children = List(first, second)
  }

  class MapParser[Result, NewResult](original: EditorParser[Result], f: Result => NewResult) extends EditorParserBase[NewResult] {
    override def parseInternal(input: Input, state: ParseState): ParseResult[NewResult] = {
      original.parse(input, state).map(f)
    }

    override def getDefault(cache: DefaultCache): Option[NewResult] = cache(original).map(f)

    override def children = List(original)
  }

  final case class EditorParseResult[+Result](successOption: Option[Success[Result]], biggestFailure: OptionFailure[Result])
    extends UnambiguousParseResult[Result] with EditorResult [Result] {

    def resultOption: Option[Result] = successOption.map(s => s.result).orElse(biggestFailure.partialResult)
    def remainder: Input = getSuccessRemainder.getOrElse(biggestFailure.asInstanceOf[ParseFailure[Result]].remainder)

    def addDefault[Other >: Result](value: Other) = biggestFailure match {
      case NoFailure => this
      case f: ParseFailure[Result] => EditorParseResult(successOption, f.addDefault(value))
    }

    def updateRemainder(f: Input => Input): EditorParseResult[Result] = {
      EditorParseResult(successOption.map(s => Success(s.result, f(s.remainder))), biggestFailure.mapRemainder(f))
    }

    override def getSuccessRemainder = successOption.map(s => s.remainder)

    override def map[NewResult](f: Result => NewResult): ParseResult[NewResult] = {

      val failure = biggestFailure match {
        case failure: ParseFailure[Result] =>
          ParseFailure(failure.partialResult.map(r => f(r)), failure.remainder, failure.message)
        case NoFailure => NoFailure
      }
      EditorParseResult(successOption.map(s => s.map(f)), failure)
    }

    override def flatMap[NewResult](f: Success[Result] => EditorParseResult[NewResult]): ParseResult[NewResult] = {

      val failure = biggestFailure match {
        case failure: ParseFailure[Result] =>
          val newResult = failure.partialResult.flatMap(r => f(Success(r, failure.remainder)).resultOption)
          ParseFailure(newResult, failure.remainder, failure.message)
        case NoFailure => NoFailure
      }
      successOption.map(s => f(s).addFailure(failure)).getOrElse(EditorParseResult(None, failure))
    }

    def biggestRealFailure: Option[ParseFailure[Result]] = biggestFailure match {
      case failure: ParseFailure[Result] => Some(failure)
      case _ => None
    }

    def addFailure[Other >: Result](other: OptionFailure[Other]): EditorParseResult[Other] =
      if (biggestFailure.offset >= other.offset || successOption.exists(s => s.remainder.offset >= other.offset)) this else
        EditorParseResult(successOption, other)
  }

}
