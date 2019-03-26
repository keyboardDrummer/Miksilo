package core.parsers2

import core.parsers.editorParsers.EditorParserWriter
import core.parsers.strings.CommonParserWriter
import langserver.types.Position

trait CommonStringReaderParser extends CommonParserWriter with EditorParserWriter {
  type Input = StringReader

  class StringReader(array: ArrayCharSequence, offset: Int, position: Position) extends StringReaderBase(array, offset, position) {

    def this(value: String) {
      this(value.toCharArray, 0, Position(0, 0))
    }

    def drop(amount: Int): StringReader = new StringReader(array, offset + amount, move(amount))
  }
}
