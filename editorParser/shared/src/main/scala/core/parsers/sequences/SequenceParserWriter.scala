package core.parsers.sequences

import core.parsers.core.{Container, Metrics, NoMetrics, Processor}
import core.parsers.editorParsers._

trait SequenceParserWriter extends CorrectingParserWriter {
  type Elem
  type Input <: SequenceInput[Input, Elem]

  case class Fail[Result](value: Option[Result], message: String, penalty: Double)
    extends ParserBuilderBase[Result] with LeafParser[Result] {

    override def getParser(text: Container[ArrayCharSequence], recursive: GetParser): BuiltParser[Result] = {
      (input, _) => newFailure(value, input, History.error(FatalError(input, message, penalty)))
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

    override def getParser(text: Container[ArrayCharSequence], recursive: GetParser): BuiltParser[Result] = {
      val parseOriginal = recursive(original)

      class DroppingParser extends BuiltParser[Result] {

        def parse(input: Input, state: ParseState, mayFail: Boolean): ParseResult[Result] = {
          var originalResult = parseOriginal(input, state)
          if (input.atEnd(text.value))
            return originalResult

          if (!mayFail) {
            originalResult = originalResult.flatMapReady(ready => {
              if (ready.remainder == input)
                SREmpty.empty
              else
                singleResult(ready)
            }, uniform = true)
          }

          val droppedInput = input.drop(text.value, 1)
          val dropError = DropError(text.value, input, droppedInput)
          val dropHistory = History.error(dropError)
          val withDrop = singleResult(new DelayedParseResult(input, dropHistory , () => {
            parse(droppedInput, state, mayFail = false).addHistory(dropHistory)
          }))
          originalResult.merge(withDrop)
        }

        override def apply(input: Input, state: ParseState): ParseResult[Result] = {
          parse(input, state, mayFail = true)
        }
      }
      new DroppingParser
    }

  }

  case class DropError(text: ArrayCharSequence, from: Input, to: Input) extends ParseError[Input] {
    def this(text: ArrayCharSequence, from: Input, expectation: String) = this(text, from, from.drop(text, 1))

    override def fix = Some(Fix("Remove unexpected symbols", TextEdit(SourceRange(from.position, to.position), "")))

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
    override def getParser(text: Container[ArrayCharSequence], recursive: GetParser): BuiltParser[Result] = {

      val parseOriginal = recursive(original)
      (input, state) => {
        val originalResult = parseOriginal.apply(input, state)
        originalResult.mapReady(r => {
          val history = History.error(MissingInput(text.value, input, r.remainder, s"<$name>", " ", History.insertFallbackPenalty))
          ReadyParseResult(r.resultOption, r.remainder, history)
        }, uniform = true)
      }
    }

    override def getMustConsume(cache: ConsumeCache) = false
  }

  case class MissingInput(text: ArrayCharSequence,
                          from: Input,
                          to: Input,
                          expectation: String,
                          insertFix: String = "",
                          penalty: Double = History.missingInputPenalty)
    extends ParseError[Input] {

    def this(text: ArrayCharSequence, from: Input, expectation: String, penalty: Double) {
      this(text, from, from.safeIncrement(text), expectation, "", penalty)
    }
    def this(text: ArrayCharSequence, from: Input, expectation: String, insertFix: String, penalty: Double) {
      this(text, from, from.safeIncrement(text), expectation, insertFix, penalty)
    }
    def this(text: ArrayCharSequence, from: Input, expectation: String, insertFix: String) {
      this(text, from, expectation, insertFix, History.missingInputPenalty)
    }
    def this(text: ArrayCharSequence, from: Input, expectation: String) {
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
      else
        Some(Fix("Insert missing symbols", TextEdit(SourceRange(from.position, from.position), trimmed)))
    }
  }

  case class ParseWholeInput[Result](original: Parser[Result])
    extends ParserBuilderBase[Result] with ParserWrapper[Result] {

    override def getParser(textContainer: Container[ArrayCharSequence], recursive: GetParser): BuiltParser[Result] = {
      val parseOriginal = recursive(original)

      new BuiltParser[Result] {
        override def apply(input: Input, state: ParseState) = {
          val result = parseOriginal(input, state)
          result.mapReady(parseResult => {
            val remainder = parseResult.remainder
            if (remainder.atEnd(textContainer.value))
              parseResult
            else {
              val error = DropError(textContainer.value, remainder, remainder.end(textContainer.value))
              ReadyParseResult(parseResult.resultOption, remainder.end(textContainer.value), parseResult.history.addError(error))
            }
          }, uniform = false)
        }
      }
    }
  }

  def elem(predicate: Elem => Boolean, kind: String) = ElemPredicate(predicate, kind)

  case class ElemPredicate(predicate: Elem => Boolean, kind: String)
    extends ParserBuilderBase[Elem] with LeafParser[Elem] {

    override def getParser(textContainer: Container[ArrayCharSequence], recursive: GetParser): BuiltParser[Elem] = {

      def apply(input: Input, state: ParseState): ParseResult[Elem] = {
        if (input.atEnd(textContainer.value)) {
          return newFailure(new MissingInput(textContainer.value, input, kind, "", History.missingInputPenalty))
        }

        val char = input.head(textContainer.value)
        if (predicate(char)) {
          newSuccess(char, input.tail(textContainer.value), History.successValue)
        }
        else
          newFailure(new MissingInput(textContainer.value, input, kind, "", History.missingInputPenalty))
      }

      apply
    }

    override def getMustConsume(cache: ConsumeCache) = true
  }

  case class FilterError[Result](from: Input, to: Input, message: String) extends ParseError[Input] {
    override def penalty = History.missingInputPenalty
  }

  case class FilterMap[Other, Result <: Other, NewResult](
                                                           original: Parser[Result],
                                                           map: Other => Either[String, NewResult])
    extends ParserBuilderBase[NewResult] with ParserWrapper[NewResult] {

    override def getParser(text: Container[ArrayCharSequence], recursive: GetParser): BuiltParser[NewResult] = {
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
                    ready.history.addError(FilterError(input, ready.remainder, message)))
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

    override def getParser(textContainer: Container[ArrayCharSequence], recursive: GetParser): BuiltParser[Result] = {
      val parseOriginal = recursive(original)
      (input, state) => {
        val originalResult = parseOriginal(input, state)
        originalResult.mapReady(ready => {
          ready.resultOption match {
            case Some(result) =>
              if (predicate(result))
                ready
              else ReadyParseResult(None, ready.remainder,
                ready.history.addError(FilterError(input, ready.remainder, getMessage(result))))
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
      val parserAndCaches = compile(this.parser).buildParser(this.parser)
      new SingleResultParser[Result, Input] {

        override def parse(text: String, mayStop: StopFunction, metrics: Metrics) = {
          parserAndCaches.textContainer.value = text.toCharArray
          findBestParseResult(parserAndCaches.parser, mayStop, metrics)
        }

        override def reset(): Unit = {
          parserAndCaches.caches.foreach(cache => cache.clear())
        }

        override def changeRange(from: Int, until: Int, insertionLength: Int, text: ArrayCharSequence): Unit = {
          if (insertionLength - (until - from) > 0) {
            parserAndCaches.textContainer.value = text
          }
          parserAndCaches.caches.foreach(cache => cache.change(from, until, insertionLength))
        }
      }
    }

    def getWholeInputParser: SingleResultParser[Result, Input] = {
      ParseWholeInput(parser).getSingleResultParser
    }

    def withRange[Other](addRange: (Input, Input, Result) => Other): Parser[Other] = {
      WithRangeParser(parser, addRange)
    }
  }
}

trait SingleResultParser[+Result, Input] {
  def reset(): Unit
  def changeRange(start: Int, end: Int, insertionLength: Int, text: ArrayCharSequence): Unit
  def parse(text: String,
            mayStop: StopFunction = StopImmediately,
            metrics: Metrics = NoMetrics): SingleParseResult[Result, Input]
}
