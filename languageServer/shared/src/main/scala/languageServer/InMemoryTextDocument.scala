package languageServer

import core.language.TextChangeHandler
import core.parsers.core.ParseText
import core.parsers.editorParsers.{Position, SourceRange}
import jsonRpc.LazyLogging
import languageServer.InMemoryTextDocument._
import lsp.TextDocumentContentChangeEvent

import scala.collection.mutable.ArrayBuffer

// Normally each node has a value, and the node to the left has a smaller value, and to the right has a larger value.

// Problem is vrij simple, de absolute offsets in the cache kwijt raken. Relative offset zou zijn.. alles in een binary tree stoppen?
// Elke node heeft absolute start offset, die berekent wordt op basis van de absolute offset van zijn parent + een modifier.
// Als we ergens text inserten
// Er zijn meerdere ASTs als result mogelijk, dus de cache kan niet 1 AST reflecten.

object InMemoryTextDocument {
  val newLine = "\n"
}

class InMemoryTextDocument(uri: String) extends LazyLogging {
  val parseText = new ParseText

  def this(uri: String, contents: String) = {
    this(uri)
    parseText.arrayOfChars = contents.toCharArray
  }

  def applyUnsafeChanges(changes: Seq[TextDocumentContentChangeEvent], handlerOption: Option[TextChangeHandler] = None): Unit = {
    try {
      applyChanges(changes, handlerOption)
    } catch {
      case error: IllegalArgumentException =>
        logger.error("Failed to apply changes because: " + error.getMessage)
    }
  }

  def applyChanges(changes: Seq[TextDocumentContentChangeEvent], handlerOption: Option[TextChangeHandler] = None): Unit = {
    for(change <- changes) {
      change.range match {
        case None =>
          parseText.arrayOfChars = changes.head.text.toCharArray
        case Some(range) =>
          parseText.applyRangeChange(change.text, range)
          handlerOption.foreach(handler =>
            handler.handleChange(parseText.getOffset(range.start), parseText.getOffset(range.end), change.text))
      }
    }
  }

  def mkString = new String(parseText.arrayOfChars)
}
