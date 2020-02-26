package core.parsers.strings

import core.parsers.editorParsers.LeftRecursiveCorrectingParserWriter

trait DefaultIndentationSensitiveWriter extends IndentationSensitiveParserWriter with LeftRecursiveCorrectingParserWriter {
  type Input = IndentationReader

  override def startInput(offsetManager: OffsetManager) = new IndentationReader(offsetManager.getOffsetNode(0), 0)

  type CacheKey = (BuiltParser[_], Set[BuiltParser[Any]], Int)

  class IndentationReader(offsetNode: OffsetNode, val indentation: Int)
    extends StringReaderBase(offsetNode) with IndentationReaderLike {

    def drop(amount: Int): IndentationReader = new IndentationReader(offsetNode.drop(amount), indentation)

    override def hashCode(): Int = offset ^ indentation

    override def equals(obj: Any): Boolean = obj match {
      case other: IndentationReader => offset == other.offset && indentation == other.indentation
      case _ => false
    }

    override def withIndentation(value: Int) = new IndentationReader(offsetNode, value)

    override def createCacheKey(parser: BuiltParser[_], state: Set[BuiltParser[Any]]) = (parser, state, indentation)
  }
}
