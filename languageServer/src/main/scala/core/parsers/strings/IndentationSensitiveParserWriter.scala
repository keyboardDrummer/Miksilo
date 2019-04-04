package core.parsers.strings

import core.parsers.editorParsers.DefaultCache

trait IndentationSensitiveParserWriter extends StringParserWriter {
  type Input <: IndentationReaderLike

  trait IndentationReaderLike extends StringReaderLike {
    def indentation: Int

    def withIndentation(value: Int): Input
  }

  case class WithIndentation[Result](inner: EditorParser[Result]) extends EditorParserBase[Result] {
    override def parseInternal(input: Input) = {
      val previous = input.indentation
      val newInput = input.withIndentation(input.position.character)
      val result: ParseResult[Result] = inner.parseInternal(newInput)
      result.updateRemainder(remainder => {
        remainder.withIndentation(previous)
      })
    }

    override def getDefault(cache: DefaultCache): Option[Result] = inner.getDefault(cache)

    override def children = List(inner)
  }

  def alignedList[Element](element: EditorParser[Element]): Self[List[Element]] = {
    aligned(element, List.empty, (a: Element, b: List[Element]) => a :: b)
  }

  def aligned[Element, Sum](element: EditorParser[Element], zero: Sum, reduce: (Element, Sum) => Sum): Self[Sum] = {
    val many = equal(element).many(zero, reduce)
    WithIndentation(leftRight(element, many, reduce))
  }

  def equal[Result](inner: EditorParser[Result]) = CheckIndentation(delta => delta == 0, "equal to", inner)
  def greaterThan[Result](inner: EditorParser[Result]) = CheckIndentation(delta => delta > 0, "greater than", inner)

  case class CheckIndentation[Result](deltaPredicate: Int => Boolean, property: String, inner: EditorParser[Result]) extends EditorParserBase[Result] {
    override def parseInternal(input: Input) = {
      val delta = input.position.character - input.indentation
      if (input.atEnd || deltaPredicate(delta)) {
        inner.parse(input)
      } else {
        newFailure(input, s"indentation ${input.position.character} of character '${input.head}' must be $property ${input.indentation}")
      }
    }

    override def getDefault(cache: DefaultCache): Option[Result] = inner.getDefault(cache)

    override def children = List(inner)
  }
}
