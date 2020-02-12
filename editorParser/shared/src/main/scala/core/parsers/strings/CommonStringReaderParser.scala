package core.parsers.strings

import core.parsers.core.Container
import core.parsers.editorParsers.Position

trait CommonStringReaderParser extends CommonParserWriter {
  type Input = StringReader

  override def startInput = new StringReader(0, Position(0, 0))

  class StringReader(offset: Int, position: Position)
    extends StringReaderBase[Input](offset, position) {

    def drop(text: ArrayCharSequence, amount: Int): StringReader = new StringReader(offset + amount, movePosition(text, amount))

    override def decrease(array: ArrayCharSequence, amount: Int) = {
      new StringReader(offset - amount, decreasePosition(array, amount))
    }
  }
}
