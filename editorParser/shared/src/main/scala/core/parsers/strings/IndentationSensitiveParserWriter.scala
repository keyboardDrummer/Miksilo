package core.parsers.strings

import core.parsers.core.ParseText
import core.parsers.editorParsers.History

trait IndentationSensitiveParserWriter extends StringParserWriter {
  type Input <: IndentationReaderLike

  trait IndentationReaderLike extends StringReaderLike[Input] {
    def indentation: Int

    def withIndentation(value: Int): Input
  }

  case class WithIndentation[Result](original: Parser[Result])
    extends ParserBuilderBase[Result] with ParserWrapper[Result]{

    override def getParser(text: ParseText, recursive: GetParser): BuiltParser[Result] = {
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

  def alignedList[Element](element: Parser[Element]): Parser[List[Element]] = {
    aligned(element, List.empty, (a: Element, b: List[Element]) => a :: b)
  }

  def aligned[Element, Sum](firstLine: Parser[Element], zero: Sum, reduce: (Element, Sum) => Sum): Parser[Sum] = {
    val remainingLines = equal(firstLine).many(zero, reduce)
    WithIndentation(leftRight(firstLine, remainingLines, combineFold(zero, reduce)))
  }

  def equal[Result](inner: Parser[Result]) = CheckIndentation(delta => delta == 0, "equal to", inner)
  def greaterThan[Result](inner: Parser[Result]) = CheckIndentation(delta => delta > 0, "greater than", inner)

  case class IndentationError(array: ParseText, from: Input, property: String) extends NextCharError {
    override def penalty = History.indentationErrorPenalty

    override def message =
      s"indentation ${from.position.character} of character '${from.head(array)}' must be $property ${from.indentation}"
  }

  case class CheckIndentation[Result](deltaPredicate: Int => Boolean, property: String, original: Parser[Result])
    extends ParserBuilderBase[Result] with ParserWrapper[Result] {

    override def getParser(text: ParseText, recursive: GetParser) = {
      val parseOriginal = recursive(original).asInstanceOf[BuiltParser[Result]]

      def apply(input: Input, state: ParseState) = {
        val delta = input.position.character - input.indentation
        if (input.atEnd(text) || deltaPredicate(delta)) {
          parseOriginal(input, state)
        } else {
          newFailure(None, input, History.error(IndentationError(text, input, property)))
        }
      }
      apply
    }

    override def leftChildren = List(original)

    override def getMustConsume(cache: ConsumeCache) = cache(original)
  }
}
