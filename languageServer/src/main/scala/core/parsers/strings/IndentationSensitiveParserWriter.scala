package core.parsers.strings

import core.parsers.editorParsers.{History, ParseError}


trait IndentationSensitiveParserWriter extends StringParserWriter {
  type Input <: IndentationReaderLike

  trait IndentationReaderLike extends StringReaderLike[Input] {
    def indentation: Int

    def withIndentation(value: Int): Input
  }

  case class WithIndentation[Result](original: Self[Result])
    extends ParserBuilderBase[Result] with ParserWrapper[Result]{

    override def getParser(recursive: GetParser): Parser[Result] = {
      val parseOriginal = recursive(original)

      def apply(input: Input, state: ParseState) = {
        val previous = input.indentation
        val newInput = input.withIndentation(input.position.character)
        val result: ParseResult[Result] = parseOriginal(newInput, state)
        result.updateRemainder(remainder => {
          remainder.withIndentation(previous)
        })
      }

      apply
    }
  }

  def alignedList[Element](element: Self[Element]): Self[List[Element]] = {
    aligned(element, List.empty, (a: Element, b: List[Element]) => a :: b)
  }

  def aligned[Element, Sum](firstLine: Self[Element], zero: Sum, reduce: (Element, Sum) => Sum): Self[Sum] = {
    val remainingLines = equal(firstLine).many(zero, reduce)
    WithIndentation(leftRight(firstLine, remainingLines, combineFold(zero, reduce)))
  }

  def equal[Result](inner: Self[Result]) = CheckIndentation(delta => delta == 0, "equal to", inner)
  def greaterThan[Result](inner: Self[Result]) = CheckIndentation(delta => delta > 0, "greater than", inner)

  case class IndentationError(from: Input, property: String) extends NextCharError {
    override def penalty = History.indentationErrorPenalty

    override def message =
      s"indentation ${from.position.character} of character '${from.head}' must be $property ${from.indentation}"
  }

  case class CheckIndentation[Result](deltaPredicate: Int => Boolean, property: String, original: Self[Result])
    extends ParserBuilderBase[Result] with ParserWrapper[Result] {

    override def getParser(recursive: GetParser) = {
      val parseOriginal = recursive(original).asInstanceOf[Parser[Result]]

      def apply(input: Input, state: ParseState) = {
        val delta = input.position.character - input.indentation
        if (input.atEnd || deltaPredicate(delta)) {
          parseOriginal(input, state)
        } else {
          newFailure(None, input, History.error(IndentationError(input, property)))
        }
      }
      apply
    }

    override def leftChildren = List(original)

    override def getMustConsume(cache: ConsumeCache) = cache(original)
  }
}
