package core.parsers.strings

import core.parsers.editorParsers.DefaultCache

trait IndentationSensitiveParserWriter extends StringParserWriter {
  type Input <: ReaderLike

  trait ReaderLike extends StringReaderLike {
    def indentation: Int

    def withIndentation(value: Int): Input
  }

  case class IndentationLess[Result](inner: EditorParser[Result]) extends EditorParser[Result] {
    override def parseInternal(input: Input, state: ParseStateLike): ParseResult[Result] = {
      val previous = input.indentation
      val newInput = input.withIndentation(0)
      val result: ParseResult[Result] = inner.parseInternal(newInput, state)
      result.updateRemainder(remainder => {
        remainder.withIndentation(previous)
      })
    }

    override def getDefault(cache: DefaultCache): Option[Result] = inner.getDefault(cache)
  }

  case class SetIndentation[Result](inner: EditorParser[Result]) extends EditorParser[Result] {
    override def parseInternal(input: Input, state: ParseStateLike): ParseResult[Result] = {
      val newInput = input.withIndentation(input.position.character)
      inner.parseInternal(newInput, state)
    }

    override def getDefault(cache: DefaultCache): Option[Result] = inner.getDefault(cache)
  }

  def aligned[Element, Sum](element: EditorParser[Element], zero: Sum, reduce: (Element, Sum) => Sum): Self[Sum] = {
    val many = element.many(zero, reduce)
    choice(leftRight(SetIndentation(element), many, reduce), succeed(zero), leftIsAlwaysBigger = true)
  }

  case class WithIndentation[Result](deltaPredicate: Int => Boolean, inner: EditorParser[Result]) extends EditorParser[Result] {
    override def parseInternal(input: Input, state: ParseStateLike): ParseResult[Result] = {
      val delta = input.position.character - input.indentation
      if (deltaPredicate(delta)) {
        state.parse(inner, input)
      } else {
        newFailure(input, s"indentation ${input.position.character} must be ... ${input.indentation}")
      }
    }

    override def getDefault(cache: DefaultCache): Option[Result] = inner.getDefault(cache)
  }
}
