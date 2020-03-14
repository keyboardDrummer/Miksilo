package languageServer

import miksilo.editorParser.LazyLogging
import core.language.DocumentEventListener
import miksilo.editorParser.parsers.core.ParseText
import miksilo.lspprotocol.lsp.TextDocumentContentChangeEvent

object InMemoryTextDocument {
  val newLine = "\n"
}

class InMemoryTextDocument(uri: String) extends LazyLogging {
  val parseText = new ParseText

  def this(uri: String, contents: String) = {
    this(uri)
    parseText.arrayOfChars = contents.toCharArray
  }

  def applyUnsafeChanges(changes: Seq[TextDocumentContentChangeEvent], handlerOption: Option[DocumentEventListener] = None): Unit = {
    try {
      applyChanges(changes, handlerOption)
    } catch {
      case error: IllegalArgumentException =>
        logger.error("Failed to apply changes because: " + error.getMessage)
    }
  }

  def applyChanges(changes: Seq[TextDocumentContentChangeEvent], handlerOption: Option[DocumentEventListener] = None): Unit = {
    for(change <- changes) {
      change.range match {
        case None =>
          val previousLength = parseText.arrayOfChars.length
          parseText.arrayOfChars = changes.head.text.toCharArray
          handlerOption.foreach(handler =>
            handler.handleChange(0, previousLength, changes.head.text))
        case Some(range) =>
          val from = parseText.getOffset(range.start)
          val until = parseText.getOffset(range.end)
          if (from > until || from < 0 || until > parseText.arrayOfChars.length) {
            throw new IllegalArgumentException(s"Range $range is outside of the document bounds")
          }
          parseText.applyRangeChange(from, until, change.text)
          handlerOption.foreach(handler => handler.handleChange(from, until, change.text))
      }
    }
  }

  def mkString = new String(parseText.arrayOfChars)
}
