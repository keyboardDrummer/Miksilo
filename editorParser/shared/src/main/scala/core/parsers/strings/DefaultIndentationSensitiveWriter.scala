package core.parsers.strings

import core.parsers.core.ParseText
import core.parsers.editorParsers.Position

trait DefaultIndentationSensitiveWriter extends IndentationSensitiveParserWriter {
  type Input = IndentationReader

  override def startInput = new IndentationReader(0, Position(0,0), 0)

  class IndentationReader(offset: Int, position: Position, val indentation: Int)
    extends StringReaderBase[Input](offset, position) with IndentationReaderLike {

    def drop(text: ParseText, amount: Int): IndentationReader = new IndentationReader(offset + amount, movePosition(text, amount), indentation)

    override def hashCode(): Int = offset ^ indentation

    override def equals(obj: Any): Boolean = obj match {
      case other: IndentationReader => offset == other.offset && indentation == other.indentation
      case _ => false
    }

    override def withIndentation(value: Int) = new IndentationReader(offset, position, value)
  }
}
