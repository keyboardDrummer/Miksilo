package miksilo.editorParser.parsers.sequences

import miksilo.editorParser.parsers.core._
import miksilo.editorParser.parsers.editorParsers._

trait SequenceParserWriter extends CorrectingParserWriter {

  case class Fail[Result](value: Option[Result], message: String, penalty: Double)
    extends ParserBuilderBase[Result] with LeafParser[Result] {

    override def getParser(recursive: GetParser): BuiltParser[Result] = {
      new BuiltParser[Result] {
        override def apply(position: TextPointer, state: State, fixPointState: FixPointState): ParseResult[Result] =
          newFailure(value, position, state, History.error(FatalError(position, message, penalty)))

        override def origin: Option[ParserBuilder[Result]] = Some(Fail.this)
      }
    }

    override def getMustConsume(cache: ConsumeCache) = false
  }

  def many[Result, Sum](element: ParserBuilder[Result],
                        zero: Sum, append: (Result, Sum) => Sum,
                        parseGreedy: Boolean = true) = {
    lazy val infinite: Parser[Sum] = new Lazy(choice(leftRight(infinite, element,
      (a, b) => combineFold(zero, append)(b,a)), succeed(zero), firstIsLonger = parseGreedy))
    infinite
  }

  // Why can't the drop be done after the original, then it wouldn't need this tricky mayFail mechanism?
  case class DropParser[Result](original: Parser[Result]) extends ParserBuilderBase[Result] with ParserWrapper[Result] {

    override def getParser(recursive: GetParser): BuiltParser[Result] = {
      val parseOriginal = recursive(original)

      class DroppingParser extends BuiltParser[Result] {

        def parse(position: TextPointer, state: State, fixPointState: FixPointState, mayFail: Boolean): ParseResult[Result] = {
          var originalResult = parseOriginal(position, state, fixPointState)
          if (position.atEnd())
            return originalResult

          if (!mayFail) {
            originalResult = originalResult.flatMapReady(ready => {
              if (ready.remainder == position)
                SREmpty.empty
              else
                singleResult(ready)
            }, uniform = true)
          }

          val droppedInput = position.drop(1)
          val dropError = DropError(position, droppedInput)
          val dropHistory = History.error(dropError)
          val withDrop = singleResult(new DelayedParseResult(position, dropHistory , () => {
            parse(droppedInput, state, fixPointState, mayFail = false).addHistory(dropHistory)
          }))
          originalResult.merge(withDrop)
        }

        override def apply(position: TextPointer, state: State, fixPointState: FixPointState): ParseResult[Result] = {
          parse(position, state, fixPointState, mayFail = true)
        }

        override def origin: Option[ParserBuilder[Result]] = Some(DropParser.this)
      }
      new DroppingParser
    }

  }

  case class DropError(from: TextPointer, to: TextPointer) extends ParseError {
    def this(from: TextPointer, expectation: String) = this(from, from.drop(1))

    override def fix = {
      val range = SourceRange(from.lineCharacter, to.lineCharacter)
      Some(Fix("Remove unexpected symbols", TextEdit(range, "")))
    }

    override def append(next: ParseError): Option[ParseError] = {
      next match {
        case drop: DropError if drop.from.offset == to.offset =>
          Some(DropError(from, drop.to))
        case _ => None
      }
    }

    override def penalty = {
      val length = to.offset - from.offset
      History.dropMaxPenalty - History.dropReduction / (History.dropLengthShift + length)
    }

    override def message = {
      val found = from.printRange(to)
      s"Did not expect '$found'"
    }

    override def canMerge = true
  }

  case class Fallback[Result](original: Parser[Result], name: String) extends ParserBuilderBase[Result] with LeafParser[Result] { // TODO combine with failure?
    override def getParser(recursive: GetParser): BuiltParser[Result] = {

      val parseOriginal = recursive(original)
      new BuiltParser[Result] {
        override def apply(position: TextPointer, state: State, fixPointState: FixPointState): ParseResult[Result] = {
          val originalResult = parseOriginal.apply(position, state, fixPointState)
          originalResult.mapReady(r => {
            val history = History.error(MissingInput(position, r.remainder, s"<$name>", " ", History.insertFallbackPenalty))
            new ReadyParseResult(r.resultOption, r.remainder, r.state, history)
          }, uniform = true)
        }

        override def origin: Option[ParserBuilder[Result]] = Some(Fallback.this)
      }
    }

    override def getMustConsume(cache: ConsumeCache) = false
  }

  case class MissingInput(from: TextPointer,
                          to: TextPointer,
                          expectation: String,
                          insertFix: String = "",
                          penalty: Double = History.missingInputPenalty)
    extends ParseError {

    def this(from: TextPointer, expectation: String, penalty: Double) {
      this(from, from.safeIncrement, expectation, "", penalty)
    }

    def this(from: TextPointer, expectation: String, insertFix: String, penalty: Double) {
      this(from, from.safeIncrement, expectation, insertFix, penalty)
    }
    def this(from: TextPointer, expectation: String, insertFix: String) {
      this(from, expectation, insertFix, History.missingInputPenalty)
    }
    def this(from: TextPointer, expectation: String) {
      this(from, expectation, "")
    }

    override def message: String = s"expected '$expectation'"

    override def toString: String = {
      val found = if (from.atEnd()) {
        "end of source"
      } else
        from.printRange(to)

      s"$message but found '$found'"
    }

    override def canMerge: Boolean = true

    override def append(next: ParseError) = {
      next match {
        case next: MissingInput if next.from.offset == from.offset =>
          val max = Math.max(penalty, next.penalty)
          val min = Math.min(penalty, next.penalty)
          val newPenalty = max + min * 0.5
          Some(new MissingInput(from, expectation + next.expectation, insertFix + next.insertFix, newPenalty))
        case _ => None
      }
    }

    override def fix: Option[Fix] = {
      val trimmed = insertFix.trim
      if (trimmed == "")
        None
      else {
        val position = from.lineCharacter
        Some(Fix("Insert missing symbols", TextEdit(SourceRange(position, position), trimmed)))
      }
    }
  }

  case class ParseWholeInput[Result](original: Parser[Result])
    extends ParserBuilderBase[Result] with ParserWrapper[Result] {

    override def getParser(recursive: GetParser): BuiltParser[Result] = {
      val parseOriginal = recursive(original)

      new BuiltParser[Result] {
        override def apply(position: TextPointer, state: State, fixPointState: FixPointState) = {
          val result = parseOriginal(position, state, fixPointState)
          result.mapReady(parseResult => {
            val remainder = parseResult.remainder
            if (remainder.atEnd())
              parseResult
            else {
              val error = DropError(remainder, remainder.end())
              new ReadyParseResult(parseResult.resultOption, remainder.end(), parseResult.state, parseResult.history.addError(error))
            }
          }, uniform = false)
        }

        override def origin: Option[ParserBuilder[Result]] = Some(ParseWholeInput.this)
      }
    }
  }

  def elem(predicate: Char => Boolean, kind: String) = ElemPredicate(predicate, kind)

  case class ElemPredicate(predicate: Char => Boolean, kind: String)
    extends ParserBuilderBase[Char] with LeafParser[Char] {

    override def getParser(recursive: GetParser): BuiltParser[Char] = {
      new BuiltParser[Char] {
        override def apply(position: TextPointer, state: State, fixPointState: FixPointState): ParseResult[Char] = {
          if (position.atEnd()) {
            return newFailure(position, state, new MissingInput(position, kind, "", History.missingInputPenalty))
          }

          val char = position.charAt(position.offset)
          if (predicate(char)) {
            newSuccess(char, position.drop(1), state, History.successValue)
          }
          else
            newFailure(position, state, new MissingInput(position, kind, "", History.missingInputPenalty))
        }

        override def origin: Option[ParserBuilder[Char]] = Some(ElemPredicate.this)
      }
    }

    override def getMustConsume(cache: ConsumeCache) = true
  }

  case class FilterError[Result](from: TextPointer, to: TextPointer, message: String) extends ParseError {
    override def penalty = History.missingInputPenalty
  }

  case class FilterMap[Other, Result <: Other, NewResult](
                                                           original: Parser[Result],
                                                           map: Other => Either[String, NewResult])
    extends ParserBuilderBase[NewResult] with ParserWrapper[NewResult] {

    override def getParser(recursive: GetParser): BuiltParser[NewResult] = {
      val parseOriginal = recursive(original)
      new BuiltParser[NewResult] {
        override def apply(position: TextPointer, state: State, fixPointState: FixPointState): ParseResult[NewResult] = {
          val originalResult = parseOriginal(position, state, fixPointState)
          originalResult.mapReady(ready => {
            ready.resultOption match {
              case Some(result) =>
                val newResultOption = map(result)
                newResultOption match {
                  case Left(message) =>
                    val leftHistory = ready.history.addError(FilterError(position, ready.remainder, message))
                    new ReadyParseResult(None, ready.remainder, ready.state, leftHistory)
                  case Right(value) =>
                    new ReadyParseResult(Some(value), ready.remainder, ready.state, ready.history)
                }
              case None =>
                ready.asInstanceOf[ReadyParseResult[State, NewResult]]
            }
          }, uniform = false)
        }

        override def origin: Option[ParserBuilder[NewResult]] = Some(FilterMap.this)
      }
    }
  }

  case class Filter[Other, Result <: Other](original: Parser[Result],
                                            predicate: Other => Boolean,
                                            getMessage: Other => String)
    extends ParserBuilderBase[Result] with ParserWrapper[Result] {

    override def getParser(recursive: GetParser): BuiltParser[Result] = {
      val parseOriginal = recursive(original)
      new BuiltParser[Result] {
        override def apply(position: TextPointer, state: State, fixPointState: FixPointState): ParseResult[Result] = {
          val originalResult = parseOriginal(position, state, fixPointState)
          originalResult.mapReady(ready => {
            ready.resultOption match {
              case Some(result) =>
                if (predicate(result))
                  ready
                else {
                  val history = ready.history.addError(FilterError(position, ready.remainder, getMessage(result)))
                  new ReadyParseResult(None, ready.remainder, ready.state, history)
                }
              case None => ready
            }
          }, uniform = false)
        }

        override def origin: Option[ParserBuilder[Result]] = Some(Filter.this)
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

  private def prependCombine[Result]: (Option[Result], Option[Vector[Result]]) => Option[Vector[Result]] = {
    val zero = Vector.empty[Result]
    val reduce = (x: Result, xs: Vector[Result]) => xs.prepended(x)
    combineFold(zero, reduce)
  }

  private def appendCombine[Result]: (Option[Vector[Result]], Option[Result]) => Option[Vector[Result]] = {
    val zero = Vector.empty[Result]
    val reduce = (x: Result, xs: Vector[Result]) => xs.appended(x)
    (a,b) => combineFold(zero, reduce)(b,a)
  }

  implicit class SequenceParserExtensions[Result](parser: Parser[Result]) extends ParserExtensions(parser) {

    def many[Sum](zero: Sum, append: (Result, Sum) => Sum,
                  parseGreedy: Boolean = true): Parser[Sum] = SequenceParserWriter.this.many(parser, zero, append, parseGreedy)

    def * : Parser[Vector[Result]] = {
      many(Vector.empty, (h: Result, t: Vector[Result]) => t.appended(h))
    }

    def ~[Right](right: => Parser[Right]): Parser[(Result, Right)] = leftRightSimple(parser, right, (a: Result, b: Right) => (a,b))

    def ~<[Right](right: Parser[Right]): ParserBuilder[Result] = leftRight(parser, right, Processor.ignoreRight[Option[Result], Option[Right]])

    def ~>[Right](right: Parser[Right]): ParserBuilder[Right] = leftRight(parser, right, Processor.ignoreLeft[Option[Result], Option[Right]])

    def +(elementName: String): Parser[Vector[Result]] = {
      leftRight(parser, parser.*, prependCombine[Result])
    }

    def someSeparated(separator: Parser[Any], elementName: String): Parser[Vector[Result]] = {
      lazy val result: Parser[Vector[Result]] = new Lazy(
          leftRight[Vector[Result], Result, Vector[Result]](result, separator ~> parser, appendCombine[Result]) |
          parser.map(first => Vector(first)))

      result
    }

    def manySeparated(separator: Parser[Any], elementName: String): Parser[Vector[Result]] = {
      val zero = Vector.empty[Result]
      choice(succeed(zero), someSeparated(separator, elementName))
    }

    def filter[Other >: Result](predicate: Other => Boolean, getMessage: Other => String) =
      Filter(parser, predicate, getMessage)

    def getSingleResultParser(): SingleResultParser[Result] = {
      SequenceParserWriter.this.getSingleResultParser(this.parser)
    }

    def getWholeInputParser(): SingleResultParser[Result] = {
      ParseWholeInput(parser).getSingleResultParser()
    }

    def withRange[Other](addRange: (TextPointer, TextPointer, Result) => Other): Parser[Other] = {
      WithRangeParser(parser, addRange)
    }
  }

}