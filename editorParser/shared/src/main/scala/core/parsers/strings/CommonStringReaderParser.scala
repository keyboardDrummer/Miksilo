package core.parsers.strings

import core.parsers.core.ParseText
import core.parsers.editorParsers.Position

trait CommonStringReaderParser extends CommonParserWriter {
  type Input = StringReader

  override def startInput = new StringReader(0)

  class StringReader(offset: Int)
    extends StringReaderBase[Input](offset) {

    def drop(text: ParseText, amount: Int): StringReader = new StringReader(offset + amount)
  }
}
