package core.parsers.sequences

import core.parsers.core.ParseInput
import core.parsers.editorParsers.{CorrectingParserWriter, History, ParseError, SingleError}
import langserver.types.Position

trait SequenceParserWriter extends CorrectingParserWriter {
  type Elem
  type Input <: SequenceInput[Input, Elem]

  trait SequenceInput[Input, Elem] extends ParseInput {
    def head: Elem
    def tail: Input

    def drop(amount: Int): Input
    def end: Input
    def printRange(end: Input): String
    def position: Position
  }

  case class Fail[Result](value: Option[Result], message: String, penalty: Double)
    extends ParserBuilderBase[Result] with LeafParser[Result] {

    override def getParser(recursive: GetParser): Parser[Result] = {
      (input, _) => newFailure(value, input, History.error(FatalError(input, message, penalty)))
    }

    override def getMustConsume(cache: ConsumeCache) = false
  }

  override def many[Result, Sum](original: ParserBuilder[Result], zero: Sum, reduce: (Result, Sum) => Sum) = {
    lazy val result: Self[Sum] = choice(WithDefault(leftRight(original, result, reduce), zero), succeed(zero), firstIsLonger = true)
    result
  }

  case class DropParser[Result](original: Self[Result]) extends ParserBuilderBase[Result] with ParserWrapper[Result] {

    override def getParser(recursive: GetParser): Parser[Result] = {
      val parseOriginal = recursive(original)
      lazy val result = new Parser[Result] {

        override def apply(input: Input, state: ParseState): ParseResult[Result] = {
          val originalResult = parseOriginal(input, state)

          if (input.atEnd)
            return originalResult

          val droppedInput = input.drop(1)
          val dropError = DropError(input, droppedInput)
          val dropHistory = History.error(dropError)
          val withDrop = singleResult(new DelayedParseResult(dropHistory , () => {
            apply(droppedInput, state).addHistory(dropHistory)
          }))
          originalResult.merge(withDrop)
        }
      }
      result
    }

  }

  case class DropError(from: Input, to: Input) extends ParseError[Input] {
    def this(from: Input, expectation: String) = this(from, from.drop(1))

    override def append(next: MyParseError): Option[MyParseError] = {
      next match {
        case drop: DropError if drop.from == to =>
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
      s"Skipped '$found'"
    }

    override def canMerge = true
  }

  case class Fallback[Result](value: Result, name: String) extends ParserBuilderBase[Result] with LeafParser[Result] { // TODO combine with failure?
    override def getParser(recursive: GetParser): Parser[Result] = {
      (input, _) => {
        val history = History.error(new MissingInput(input, s"<$name>", History.insertFallbackPenalty))
        val result = ReadyParseResult(Some(value), input, history)
        singleResult(result)
      }
    }

    override def getMustConsume(cache: ConsumeCache) = false
  }

  case class WithDefault[Result](original: Self[Result], _default: Result)
    extends ParserBuilderBase[Result] with ParserWrapper[Result] {

    override def getParser(recursive: GetParser): Parser[Result] = {
      val parseOriginal = recursive(original)

      def apply(input: Input, state: ParseState): ParseResult[Result] = {
        val result = parseOriginal(input, state)
        result.mapReady(ready => {
          if (ready.resultOption.isEmpty || ready.remainder == input) {
            ReadyParseResult(Some(_default), ready.remainder, ready.history)
          } else
            ready
        }, uniform = true)
      }
      apply
    }
  }

  case class MissingInput(from: Input, to: Input, expectation: String, penalty: Double = History.missingInputPenalty) extends ParseError[Input] {

    def this(from: Input, expectation: String, penalty: Double) =
      this(from, if (from.atEnd) from else from.drop(1), expectation, penalty)

    override def message: String = {
      val found = if (from.atEnd) {
        "end of source"
      } else
        from.printRange(to)

      s"expected $expectation but found '$found'"
    }
  }

  case class ParseWholeInput[Result](original: Self[Result])
    extends ParserBuilderBase[Result] with ParserWrapper[Result] {

    override def getParser(recursive: GetParser): Parser[Result] = {
      val parseOriginal = recursive(original)

      new Parser[Result] {
        override def apply(input: Input, state: ParseState) = {
          val result = parseOriginal(input, state)
          result.mapReady(parseResult => {
            val remainder = parseResult.remainder
            if (remainder.atEnd)
              parseResult
            else {
              val error = DropError(remainder, remainder.end)
              ReadyParseResult(parseResult.resultOption, remainder.end, parseResult.history.addError(error))
            }
          }, uniform = false)
        }
      }
    }
  }

  def elem(predicate: Elem => Boolean, kind: String) = ElemPredicate(predicate, kind)

  case class ElemPredicate(predicate: Elem => Boolean, kind: String)
    extends ParserBuilderBase[Elem] with LeafParser[Elem] {

    override def getParser(recursive: GetParser): Parser[Elem] = {

      def apply(input: Input, state: ParseState): ParseResult[Elem] = {
        if (input.atEnd) {
          return newFailure(new MissingInput(input, kind, History.missingInputPenalty))
        }

        val char = input.head
        if (predicate(char)) {
          newSuccess(char, input.tail, History.successValue)
        }
        else
          newFailure(new MissingInput(input, kind, History.missingInputPenalty))
      }

      apply
    }

    override def getMustConsume(cache: ConsumeCache) = true
  }


  case class Filter[Other, Result <: Other](original: Self[Result],
                                            predicate: Other => Boolean,
                                            getMessage: Other => String)
    extends ParserBuilderBase[Result] with ParserWrapper[Result] {

    override def getParser(recursive: GetParser): Parser[Result] = {
      val parseOriginal = recursive(original)
      (input, state) => {
        val originalResult = parseOriginal(input, state)
        originalResult.mapReady(ready => {
          ready.resultOption match {
            case Some(result) =>
              if (predicate(result))
                ready
              else ReadyParseResult(None, ready.remainder,
                ready.history.addError(MissingInput(input, ready.remainder, getMessage(result), History.missingInputPenalty)))
            case None => ready
          }
        }, uniform = false)
      }
    }
  }

  implicit class SequenceParserExtensions[Result](parser: Self[Result]) extends ParserExtensions(parser) {

    def someSeparated(separator: Self[Any], elementName: String): Self[List[Result]] = {
      val reduce = (h: Result, t: List[Result]) => h :: t
      val zero = List.empty[Result]
      lazy val result: Self[List[Result]] = separator ~>
        (WithDefault(leftRight(DropParser(parser), DropParser(result), reduce), zero) |
          Fail(Some(zero), elementName, History.insertDefaultPenalty)) |
        succeed(zero)
      leftRight(parser, DropParser(result), reduce)
    }

    def manySeparated(separator: Self[Any], elementName: String): Self[List[Result]] = {
      val zero = List.empty[Result]
      DropParser(choice(someSeparated(separator, elementName), succeed(zero), firstIsLonger = true))
    }

    def filter[Other >: Result](predicate: Other => Boolean, getMessage: Other => String) = Filter(parser, predicate, getMessage)

    def withDefault[Other >: Result](_default: Other): Self[Other] = WithDefault(parser, _default)

    def getSingleResultParser: SingleResultParser[Result] = {
      val parser = compile(this.parser).buildParser(this.parser)
      (input, mayStop) => findBestParseResult(parser, input, mayStop)
    }

    def getWholeInputParser(): SingleResultParser[Result] = {
      ParseWholeInput(parser).getSingleResultParser
    }

    def withRange[Other >: Result](addRange: (Input, Input, Result) => Other): Self[Other] = {
      WithRangeParser(parser, addRange)
    }
  }

  trait SingleResultParser[+Result] {
    def parse(input: Input, mayStop: (Double, Double) => Boolean = (_, _) => true): SingleParseResult[Result]
    def parseUntilBetterThanNext(input: Input): SingleParseResult[Result] = {
      parse(input, (best, second) => best > second)
    }

    def parseXSteps(input: Input, steps: Int): SingleParseResult[Result] = {
      var counter = 0
      parse(input, (_, _) => {
        counter += 1
        counter == steps
      })
    }
  }
}
