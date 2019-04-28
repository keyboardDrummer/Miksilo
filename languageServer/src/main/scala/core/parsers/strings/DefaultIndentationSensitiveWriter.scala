package core.parsers.strings

import langserver.types.Position

trait DefaultIndentationSensitiveWriter extends IndentationSensitiveParserWriter {
  type Input = IndentationReader

  class IndentationReader(array: ArrayCharSequence, offset: Int, scoredPosition: ScoredPosition, val indentation: Int)
    extends StringReaderBase(array, offset, scoredPosition) with IndentationReaderLike {

    def this(value: String) {
      this(value.toCharArray, 0, ScoredPosition(0, Position(0, 0)), 0)
    }

    def drop(amount: Int): IndentationReader = new IndentationReader(array, offset + amount, move(amount), indentation)

    override def hashCode(): Int = offset ^ indentation

    override def equals(obj: Any): Boolean = obj match {
      case other: IndentationReader => offset == other.offset && indentation == other.indentation
      case _ => false
    }

    override def withIndentation(value: Int) = new IndentationReader(array, offset, scoredPosition, value)
  }
}
