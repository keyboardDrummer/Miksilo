package core.parsers.strings

import core.parsers.core.ParseText
import core.parsers.editorParsers.Position

trait CommonStringReaderParser extends CommonParserWriter {
  type Input = StringReader

  override def startInput(offsetManager: OffsetManager) = new StringReader(offsetManager.getOffsetNode(0))

  type CacheKey = (BuiltParser[_], Set[BuiltParser[Any]])
  class StringReader(offsetNode: CachingTextPointer)
    extends StringReaderBase(offsetNode) {

    def drop(amount: Int): StringReader = new StringReader(offsetNode.drop(amount))

    override def createCacheKey(parser: BuiltParser[_], state: Set[BuiltParser[Any]]) = (parser, state)
  }
}
