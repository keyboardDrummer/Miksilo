package core.parsers.strings

import core.parsers.core.{ParseText, TextPointer}
import core.parsers.editorParsers.Position

trait CommonStringReaderParser extends CommonParserWriter {
  type Input = StringReader

  override def startInput(zero: TextPointer) = new StringReader(zero)

  type CacheKey = (BuiltParser[_], Set[BuiltParser[Any]])
  class StringReader(offsetNode: TextPointer)
    extends StringReaderBase(offsetNode) {

    def drop(amount: Int): StringReader = new StringReader(offsetNode.drop(amount))

    override def createCacheKey(parser: BuiltParser[_], state: Set[BuiltParser[Any]]) = (parser, state)
  }
}
