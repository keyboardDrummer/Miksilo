package core.parsers.strings

import core.parsers.core.ParseText
import core.parsers.editorParsers.History

trait IndentationSensitiveParserWriter extends StringParserWriter {
  type Input <: IndentationReaderLike

  trait IndentationReaderLike extends StringReaderLike {
    def indentation: Int

    def withIndentation(value: Int): Input
  }

  case class WithIndentation[Result](original: Parser[Result])
    extends ParserBuilderBase[Result] with ParserWrapper[Result]{

    override def getParser(recursive: GetParser): BuiltParser[Result] = {
      val parseOriginal = recursive(original)

      def apply(input: Input, state: FixPointState) = {
        val previous = input.indentation
        val position = input.offsetNode.position
        val newInput = input.withIndentation(position.character)
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

  case class IndentationError(from: Input, property: String) extends NextCharError {
    override def penalty = History.indentationErrorPenalty

    override def message = {
      val position = from.offsetNode.position
      s"indentation ${position.character} of character '${from.head()}' must be $property ${from.indentation}"
    }
  }

  case class CheckIndentation[Result](deltaPredicate: Int => Boolean, property: String, original: Parser[Result])
    extends ParserBuilderBase[Result] with ParserWrapper[Result] {

    override def getParser(recursive: GetParser) = {
      val parseOriginal = recursive(original).asInstanceOf[BuiltParser[Result]]

      def apply(input: Input, state: FixPointState) = {
        val position = input.offsetNode.position
        val delta = position.character - input.indentation
        if (input.atEnd() || deltaPredicate(delta)) {
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
