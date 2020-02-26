package core.parsers.sequences

import core.parsers.core._
import core.parsers.editorParsers._

trait SequenceParserWriter extends CorrectingParserWriter {

  trait SequenceInput[Elem] extends CorrectingInput {

    def drop(amount: Int): Input

    def head(array: ParseText): Elem
    def tail(array: ParseText): Input

    def safeIncrement(array: ParseText): Input =
      if (atEnd(array)) this.asInstanceOf[Input]
      else drop(1)
    def end(array: ParseText): Input
    def printRange(text: ParseText, end: Input): String
  }

  type Elem
  type Input <: SequenceInput[Elem]

  case class Fail[Result](value: Option[Result], message: String, penalty: Double)
    extends ParserBuilderBase[Result] with LeafParser[Result] {

    override def getParser(text: ParseText, recursive: GetParser): BuiltParser[Result] = {
      (input, _) => newFailure(value, input, History.error(FatalError(text, input, message, penalty)))
    }

    override def getMustConsume(cache: ConsumeCache) = false
  }

  def many[Result, Sum](original: ParserBuilder[Result],
                        zero: Sum, reduce: (Result, Sum) => Sum,
                        parseGreedy: Boolean = true) = {
    lazy val result: Parser[Sum] = choice(leftRight(original, result, combineFold(zero, reduce)), succeed(zero), firstIsLonger = parseGreedy)
    result
  }

  // Why can't the drop be done after the original, then it wouldn't need this tricky mayFail mechanism?
  case class DropParser[Result](original: Parser[Result]) extends ParserBuilderBase[Result] with ParserWrapper[Result] {

    override def getParser(text: ParseText, recursive: GetParser): BuiltParser[Result] = {
      val parseOriginal = recursive(original)

      class DroppingParser extends BuiltParser[Result] {

        def parse(input: Input, state: FixPointState, mayFail: Boolean): ParseResult[Result] = {
          var originalResult = parseOriginal(input, state)
          if (input.atEnd(text))
            return originalResult

          if (!mayFail) {
            originalResult = originalResult.flatMapReady(ready => {
              if (ready.remainder == input)
                SREmpty.empty
              else
                singleResult(ready)
            }, uniform = true)
          }

          val droppedInput = input.drop(1)
          val dropError = DropError(text, input, droppedInput)
          val dropHistory = History.error(dropError)
          val withDrop = singleResult(new DelayedParseResult(input, dropHistory , () => {
            parse(droppedInput, state, mayFail = false).addHistory(dropHistory)
          }))
          originalResult.merge(withDrop)
        }

        override def apply(input: Input, state: FixPointState): ParseResult[Result] = {
          parse(input, state, mayFail = true)
        }
      }
      new DroppingParser
    }

  }

  case class DropError(text: ParseText, from: Input, to: Input) extends ParseError[Input] {
    def this(text: ParseText, from: Input, expectation: String) = this(text, from, from.drop(1))

    override def fix = {
      val range = SourceRange(text.getPosition(from.offset), text.getPosition(to.offset))
      Some(Fix("Remove unexpected symbols", TextEdit(range, "")))
    }

    override def append(next: MyParseError): Option[MyParseError] = {
      next match {
        case drop: DropError if drop.from == to =>
          Some(DropError(text, from, drop.to))
        case _ => None
      }
    }

    override def penalty = {
      val length = to.offset - from.offset
      History.dropMaxPenalty - History.dropReduction / (History.dropLengthShift + length)
    }

    override def message = {
      val found = from.printRange(text, to)
      s"Did not expect '$found'"
    }

    override def canMerge = true
  }

  case class Fallback[Result](original: Parser[Result], name: String) extends ParserBuilderBase[Result] with LeafParser[Result] { // TODO combine with failure?
    override def getParser(text: ParseText, recursive: GetParser): BuiltParser[Result] = {

      val parseOriginal = recursive(original)
      (input, state) => {
        val originalResult = parseOriginal.apply(input, state)
        originalResult.mapReady(r => {
          val history = History.error(MissingInput(text, input, r.remainder, s"<$name>", " ", History.insertFallbackPenalty))
          ReadyParseResult(r.resultOption, r.remainder, history)
        }, uniform = true)
      }
    }

    override def getMustConsume(cache: ConsumeCache) = false
  }

  case class MissingInput(text: ParseText,
                          from: Input,
                          to: Input,
                          expectation: String,
                          insertFix: String = "",
                          penalty: Double = History.missingInputPenalty)
    extends ParseError[Input] {

    def this(text: ParseText, from: Input, expectation: String, penalty: Double) {
      this(text, from, from.safeIncrement(text), expectation, "", penalty)
    }
    def this(text: ParseText, from: Input, expectation: String, insertFix: String, penalty: Double) {
      this(text, from, from.safeIncrement(text), expectation, insertFix, penalty)
    }
    def this(text: ParseText, from: Input, expectation: String, insertFix: String) {
      this(text, from, expectation, insertFix, History.missingInputPenalty)
    }
    def this(text: ParseText, from: Input, expectation: String) {
      this(text, from, expectation, "")
    }

    override def message: String = s"expected '$expectation'"

    override def toString: String = {
      val found = if (from.atEnd(text)) {
        "end of source"
      } else
        from.printRange(text, to)

      s"$message but found '$found'"
    }

    override def canMerge: Boolean = true

    override def append(next: ParseError[Input]) = {
      next match {
        case next: MissingInput if next.from == from =>
          val max = Math.max(penalty, next.penalty)
          val min = Math.min(penalty, next.penalty)
          val newPenalty = max + min * 0.5
          Some(new MissingInput(text, from, expectation + next.expectation, insertFix + next.insertFix, newPenalty))
        case _ => None
      }
    }

    override def fix: Option[Fix] = {
      val trimmed = insertFix.trim
      if (trimmed == "")
        None
      else {
        val position = text.getPosition(from.offset)
        Some(Fix("Insert missing symbols", TextEdit(SourceRange(position, position), trimmed)))
      }
    }
  }

  case class ParseWholeInput[Result](original: Parser[Result])
    extends ParserBuilderBase[Result] with ParserWrapper[Result] {

    override def getParser(text: ParseText, recursive: GetParser): BuiltParser[Result] = {
      val parseOriginal = recursive(original)

      new BuiltParser[Result] {
        override def apply(input: Input, state: FixPointState) = {
          val result = parseOriginal(input, state)
          result.mapReady(parseResult => {
            val remainder = parseResult.remainder
            if (remainder.atEnd(text))
              parseResult
            else {
              val error = DropError(text, remainder, remainder.end(text))
              ReadyParseResult(parseResult.resultOption, remainder.end(text), parseResult.history.addError(error))
            }
          }, uniform = false)
        }
      }
    }
  }

  def elem(predicate: Elem => Boolean, kind: String) = ElemPredicate(predicate, kind)

  case class ElemPredicate(predicate: Elem => Boolean, kind: String)
    extends ParserBuilderBase[Elem] with LeafParser[Elem] {

    override def getParser(text: ParseText, recursive: GetParser): BuiltParser[Elem] = {

      def apply(input: Input, state: FixPointState): ParseResult[Elem] = {
        if (input.atEnd(text)) {
          return newFailure(new MissingInput(text, input, kind, "", History.missingInputPenalty))
        }

        val char = input.head(text)
        if (predicate(char)) {
          newSuccess(char, input.tail(text), History.successValue)
        }
        else
          newFailure(new MissingInput(text, input, kind, "", History.missingInputPenalty))
      }

      apply
    }

    override def getMustConsume(cache: ConsumeCache) = true
  }

  case class FilterError[Result](text: ParseText, from: Input, to: Input, message: String) extends ParseError[Input] {
    override def penalty = History.missingInputPenalty
  }

  case class FilterMap[Other, Result <: Other, NewResult](
                                                           original: Parser[Result],
                                                           map: Other => Either[String, NewResult])
    extends ParserBuilderBase[NewResult] with ParserWrapper[NewResult] {

    override def getParser(text: ParseText, recursive: GetParser): BuiltParser[NewResult] = {
      val parseOriginal = recursive(original)
      (input, state) => {
        val originalResult = parseOriginal(input, state)
        originalResult.mapReady(ready => {
          ready.resultOption match {
            case Some(result) =>
              val newResultOption = map(result)
              newResultOption match {
                case Left(message) =>
                  ReadyParseResult(None, ready.remainder,
                    ready.history.addError(FilterError(text, input, ready.remainder, message)))
                case Right(value) =>
                  ReadyParseResult(Some(value), ready.remainder, ready.history)
              }
            case None =>
              ready.asInstanceOf[ReadyParseResult[Input, NewResult]]
          }
        }, uniform = false)
      }
    }
  }

  case class Filter[Other, Result <: Other](original: Parser[Result],
                                            predicate: Other => Boolean,
                                            getMessage: Other => String)
    extends ParserBuilderBase[Result] with ParserWrapper[Result] {

    override def getParser(text: ParseText, recursive: GetParser): BuiltParser[Result] = {
      val parseOriginal = recursive(original)
      (input, state) => {
        val originalResult = parseOriginal(input, state)
        originalResult.mapReady(ready => {
          ready.resultOption match {
            case Some(result) =>
              if (predicate(result))
                ready
              else ReadyParseResult(None, ready.remainder,
                ready.history.addError(FilterError(text, input, ready.remainder, getMessage(result))))
            case None => ready
          }
        }, uniform = false)
      }
    }
  }

  protected def combineFold[Result, Sum](zero: Sum, reduce: (Result, Sum) => Sum):
    (Option[Result], Option[Sum]) => Option[Sum] = {
    case (Some(x), Some(xs)) => Some(reduce(x, xs))
    case (None, xs) => xs
    case (Some(x), None) => Some(reduce(x, zero))
    case _ => None
  }

  private def combineMany[Result]: (Option[Result], Option[List[Result]]) => Option[List[Result]] = {
    val zero = List.empty[Result]
    val reduce = (x: Result, xs: List[Result]) => x :: xs
    combineFold(zero, reduce)
  }

  implicit class SequenceParserExtensions[Result](parser: Parser[Result]) extends ParserExtensions(parser) {

    def many[Sum](zero: Sum, reduce: (Result, Sum) => Sum,
                  parseGreedy: Boolean = true): Parser[Sum] = SequenceParserWriter.this.many(parser, zero, reduce, parseGreedy)

    def * : Parser[List[Result]] = {
      many(List.empty, (h: Result, t: List[Result]) => h :: t)
    }

    def ~[Right](right: => Parser[Right]): Parser[(Result, Right)] = leftRightSimple(parser, right, (a: Result, b: Right) => (a,b))

    def ~<[Right](right: Parser[Right]): ParserBuilder[Result] = leftRight(parser, right, Processor.ignoreRight[Option[Result], Option[Right]])

    def ~>[Right](right: Parser[Right]): ParserBuilder[Right] = leftRight(parser, right, Processor.ignoreLeft[Option[Result], Option[Right]])

    def +(elementName: String): Parser[List[Result]] = {
      leftRight(parser, parser.*, combineMany[Result])
    }

    def someSeparated(separator: Parser[Any], elementName: String): Parser[List[Result]] = {
      lazy val result: Parser[List[Result]] = separator ~>
        leftRight[Result, List[Result], List[Result]](parser, result, combineMany[Result]) |
          Fail(Some(List.empty[Result]), elementName, History.insertDefaultPenalty) | // TODO can we remove this Fail?
          succeed(List.empty[Result])
      leftRight(parser, result, combineMany[Result])
    }

    def manySeparated(separator: Parser[Any], elementName: String): Parser[List[Result]] = {
      val zero = List.empty[Result]
      choice(someSeparated(separator, elementName), succeed(zero), firstIsLonger = true)
    }

    def filter[Other >: Result](predicate: Other => Boolean, getMessage: Other => String) =
      Filter(parser, predicate, getMessage)

    def getSingleResultParser: SingleResultParser[Result, Input] = {
      SequenceParserWriter.this.getSingleResultParser(this.parser)
    }

    def getWholeInputParser: SingleResultParser[Result, Input] = {
      ParseWholeInput(parser).getSingleResultParser
    }

    def withRange[Other](addRange: (OffsetNode, OffsetNode, Result) => Other): Parser[Other] = {
      WithRangeParser(parser, addRange)
    }
  }

}

trait SingleResultParser[+Result, Input <: ParseInput] {
  def reset(): Unit
  def changeRange(start: Int, end: Int, insertionLength: Int): Unit
  def resetAndParse(text: String,
                    mayStop: StopFunction = StopImmediately,
                    metrics: Metrics = NoMetrics): SingleParseResult[Result, Input] = {
    reset()
    parse(text, mayStop, metrics)
  }

  def parse(text: String,
            mayStop: StopFunction = StopImmediately,
            metrics: Metrics = NoMetrics): SingleParseResult[Result, Input]
}
