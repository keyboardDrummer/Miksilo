//package core.parsers.ambiguousEditorParsers
//
//import core.parsers.ambigousParsers.AmbiguousParserWriter
//import core.parsers.editorParsers.{DefaultCache, EditorParserWriter}
//
//trait AmbiguousEditorParserWriter extends AmbiguousParserWriter with EditorParserWriter {
//
//  object NoFailure extends OptionFailure[Nothing] {
//    override def offset: Int = -1
//
//    override def map[NewResult](f: Nothing => NewResult): OptionFailure[NewResult] = this
//
//    override def partialResult: Option[Nothing] = None
//
//    override def updateRemainder(f: Input => Input) = this
//  }
//
//  type ParseResult[+Result] = EditorParseResult[Result]
//
//  override def combineSuccesses[Result](parseResults: Seq[EditorParseResult[Result]]) =
//    EditorParseResult(parseResults.flatMap(s => s.successes).toList,
//      parseResults.map(s => s.biggestFailure).fold(NoFailure)((a, b) => a.getBiggest(b)))
//
//  override def newFailure[Result](partial: Option[Result], input: Input, errors: List[ParseError]) =
//    EditorParseResult(List.empty, ParseFailure(partial, input, errors))
//
//  override def newSuccess[Result](result: Result, remainder: Input) =
//    EditorParseResult(List(Success(result, remainder)), NoFailure)
//
//  override def newParseState(parser: EditorParser[_]) = new LeftRecursionDetectorState()
//
//  override def newFailure[Result](input: Input, message: String): EditorParseResult[Nothing] = new ParseFailure(None, input, message)
//
//  override def leftRight[Left, Right, NewResult](left: EditorParser[Left],
//                                                 right: => EditorParser[Right],
//                                                 combine: (Left, Right) => NewResult): EditorParser[NewResult] =
//    new Sequence(left, right, combine)
//
//  override def choice[Result](first: EditorParser[Result], other: => EditorParser[Result], leftIsAlwaysBigger: Boolean): EditorParser[Result] =
//    if (leftIsAlwaysBigger) new Choice(first, other) else new Choice(first, other)
//
//  override def map[Result, NewResult](original: Self[Result], f: Result => NewResult): Self[NewResult] = new MapParser(original, f)
//
//  override def parseWholeInput[Result](parser: EditorParser[Result],
//                                       input: Input): EditorParseResult[Result] = {
//    val result = parser.parseRoot(input)
//    if (!result.successful)
//      return result
//
//    val resultsAtEnd = result.successes.filter(r => r.remainder.atEnd)
//    if (resultsAtEnd.isEmpty) {
//      val best = result.successes.maxBy(r => r.remainder.offset)
//      val failedSuccess = new ParseFailure(Some(best.result), best.remainder, "Did not parse entire input")
//      failedSuccess.getBiggest(result.biggestFailure)
//    } else {
//      EditorParseResult(resultsAtEnd, result.biggestFailure)
//    }
//  }
//
//  class Sequence[+Left, +Right, Result](val left: EditorParser[Left],
//                                         _right: => EditorParser[Right],
//                                         combine: (Left, Right) => Result)
//    extends EditorParserBase[Result] with SequenceLike[Result] {
//
//    lazy val right: EditorParser[Right] = _right
//
//
//    override def getParser(recursive: GetParse) = {
//      val leftParse = recursive(left)
//      lazy val rightParse = recursive(right)
//      input: Input => {
//        val leftResult = leftParse(input)
//        val leftFailure = right.default.map(rightDefault => leftResult.biggestFailure.map(l => combine(l, rightDefault))).getOrElse(NoFailure)
//        val rightResults = leftResult.successes.map(leftSuccess => {
//          val rightResult = rightParse(leftSuccess.remainder)
//          val endSuccesses = rightResult.successes.map(rightSuccess => {
//            Success(combine(leftSuccess.result, rightSuccess.result), rightSuccess.remainder)
//          })
//          val rightFailure = rightResult.biggestFailure.map(r => combine(leftSuccess.result, r))
//          EditorParseResult(endSuccesses, rightFailure)
//        })
//        combineSuccesses(rightResults).addFailure(leftFailure)
//      }
//    }
//
//    override def getDefault(cache: DefaultCache): Option[Result] = for {
//      leftDefault <- cache(left)
//      rightDefault <- cache(right)
//    } yield combine(leftDefault, rightDefault)
//  }
//
//  class Choice[+First <: Result, +Second <: Result, Result](val first: EditorParser[First], _second: => EditorParser[Second])
//    extends EditorParserBase[Result] with ChoiceLike[Result] {
//    lazy val second = _second
//
//    override def getParser(recursive: GetParse) = {
//      val parseFirst = recursive(first)
//      lazy val parseSecond = recursive(second)
//
//      def apply(input: Input) = {
//        val firstResult = parseFirst(input)
//        val secondResult = parseSecond(input)
//        val result = EditorParseResult[Result](firstResult.successes ++ secondResult.successes,
//          firstResult.biggestFailure.getBiggest(secondResult.biggestFailure))
//        result
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
//
//  implicit def toResult[Result](biggestFailure: OptionFailure[Result]): EditorParseResult[Result] = EditorParseResult(List.empty, biggestFailure)
//
//  override def abort = EditorParseResult(List.empty, NoFailure)
//
//  case class EditorParseResult[+Result](successes: List[Success[Result]], biggestFailure: OptionFailure[Result])
//    extends AmbiguousParseResult[Result] with EditorResult[Result] {
//
//    override def map[NewResult](f: Result => NewResult): EditorParseResult[NewResult] = {
//      EditorParseResult[NewResult](successes.map(r => r.map(f)), biggestFailure.map(f))
//    }
//
//    def biggestRealFailure: Option[ParseFailure[Result]] = biggestFailure match {
//      case failure: ParseFailure[Result] => Some(failure)
//      case _ => None
//    }
//
//    def addFailure[Other >: Result](other: OptionFailure[Other]): EditorParseResult[Other] =
//      if (biggestFailure.offset >= other.offset) this else
//        EditorParseResult(successes, other)
//
//    def addDefault[Other >: Result](value: Other, force: Boolean = false) = biggestFailure match {
//      case NoFailure => this
//      case f: ParseFailure[Result] => EditorParseResult(successes, f.addDefault(value, force))
//    }
//
//    override def getSingleSuccesses =
//      successes.map(r => SingleSuccess(EditorParseResult(List(r), biggestFailure), r.remainder))
//
//    override def successful = successes.nonEmpty
//
//    override def flatMap[NewResult](f: Success[Result] => ParseResult[NewResult]) = {
//      val newSuccesses = successes.map(success => f(success))
//      val failure = biggestFailure match {
//        case failure: ParseFailure[Result] =>
//          val newResult = failure.partialResult.flatMap(r => f(Success(r, failure.remainder)).resultOption)
//          ParseFailure(newResult, failure.remainder, failure.errors)
//        case NoFailure => NoFailure
//      }
//      combineSuccesses(newSuccesses).addFailure(failure)
//    }
//
//    override def resultOption = successes.headOption.map(s => s.result).orElse(biggestFailure.partialResult)
//
//    override def updateRemainder(f: Input => Input) = ???
//
//    override def offset = ???
//
//    override def errors = ???
//  }
//
//}
