package core.parsers.strings

import core.parsers.core.ParseText
import core.parsers.editorParsers.Position

trait CommonStringReaderParser extends CommonParserWriter {
  type Input = StringReader

  override def startInput(offsetManager: OffsetManager) = new StringReader(offsetManager.getOffsetNode(0))

  class StringReader(offsetNode: OffsetNode)
    extends StringReaderBase(offsetNode) {

    def drop(amount: Int): StringReader = new StringReader(offsetNode.drop(amount))
  }
}
