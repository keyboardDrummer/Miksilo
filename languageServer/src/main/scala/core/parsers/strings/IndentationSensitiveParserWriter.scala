package core.parsers.strings

import core.parsers.editorParsers.DefaultCache
import langserver.types.Position

trait DefaultIndentationSensitiveWriter extends IndentationSensitiveParserWriter {
  type Input = IndentationReader

  class IndentationReader(array: ArrayCharSequence, offset: Int, position: Position, val indentation: Int)
    extends StringReaderBase(array, offset, position) with IndentationReaderLike {

    def this(value: String) {
      this(value.toCharArray, 0, Position(0, 0), 0)
    }

    def drop(amount: Int): IndentationReader = new IndentationReader(array, offset + amount,
      newPosition(position, array, offset, amount), indentation)

    override def hashCode(): Int = offset ^ indentation

    override def equals(obj: Any): Boolean = obj match {
      case other: IndentationReader => offset == other.offset && indentation == other.indentation
      case _ => false
    }

    override def withIndentation(value: Int) = new IndentationReader(array, offset, position, value)
  }
}

trait IndentationSensitiveParserWriter extends StringParserWriter {
  type Input <: IndentationReaderLike


  trait IndentationReaderLike extends StringReaderLike {
    def indentation: Int

    def withIndentation(value: Int): Input
  }

  case class WithIndentation[Result](inner: EditorParser[Result]) extends EditorParser[Result] {
    override def parseInternal(input: Input, state: ParseStateLike): ParseResult[Result] = {
      val previous = input.indentation
      val newInput = input.withIndentation(input.position.character)
      val result: ParseResult[Result] = inner.parseInternal(newInput, state)
      result.updateRemainder(remainder => {
        remainder.withIndentation(previous)
      })
    }

    override def getDefault(cache: DefaultCache): Option[Result] = inner.getDefault(cache)
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

  case class CheckIndentation[Result](deltaPredicate: Int => Boolean, property: String, inner: EditorParser[Result]) extends EditorParser[Result] {
    override def parseInternal(input: Input, state: ParseStateLike): ParseResult[Result] = {
      val delta = input.position.character - input.indentation
      if (input.atEnd || deltaPredicate(delta)) {
        state.parse(inner, input)
      } else {
        newFailure(input, s"indentation ${input.position.character} of character '${input.head}' must be $property ${input.indentation}")
      }
    }

    override def getDefault(cache: DefaultCache): Option[Result] = inner.getDefault(cache)
  }
}
