package core.parsers.strings

import languageServer.Position

trait CommonStringReaderParser extends CommonParserWriter {
  type Input = StringReader

  class StringReader(array: ArrayCharSequence, offset: Int, position: Position)
    extends StringReaderBase[Input](array, offset, position) {

    def this(value: String) {
      this(value.toCharArray, 0, Position(0, 0))
    }

    def drop(amount: Int): StringReader = new StringReader(array, offset + amount, move(amount))
  }
}
