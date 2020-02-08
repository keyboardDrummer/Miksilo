package languageServer

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

class InMemoryTextDocument(uri: String, var contents: ArrayBuffer[Array[Char]]) extends LazyLogging {
  def this(uri: String, contents: String) = {
    this(uri, ArrayBuffer[Array[Char]](
      contents.split(newLine, -1).view.map(line => line.toArray).toSeq: _*))
  }

  def applyUnsafeChanges(changes: Seq[TextDocumentContentChangeEvent]): Unit = {
    try {
      applyChanges(changes)
    } catch {
      case error: IllegalArgumentException =>
        logger.error("Failed to apply changes because: " + error.getMessage)
    }
  }

  def applyChanges(changes: Seq[TextDocumentContentChangeEvent]): Unit = {
    for(change <- changes) {
      change.range match {
        case None =>
          contents = new InMemoryTextDocument(uri, changes.head.text).contents
        case Some(range) =>
          applyRangeChange(change, range)
      }
    }
  }

  def outOfBounds(position: Position): Boolean = {
    position.line < 0 || position.line >= contents.length ||
      position.character < 0 || position.character > contents(position.line).length
  }

  private def applyRangeChange(change: TextDocumentContentChangeEvent, range: SourceRange): Unit = {
    if (outOfBounds(range.start) || outOfBounds(range.end)) {
      throw new IllegalArgumentException(s"range '$range' is out of bounds")
    }

    val newLines: Array[Array[Char]] = change.text.split(newLine, -1).map(line => line.toCharArray)
    newLines(0) = contents(range.start.line).take(range.start.character) ++ newLines(0)
    newLines(newLines.length - 1) = newLines(newLines.length - 1) ++ contents(range.end.line).drop(range.end.character)

    val rangeLineDifference = range.end.line - range.start.line + 1
    val lineReuseLength = Math.min(newLines.length, rangeLineDifference)
    for (newLineIndex <- 0.until(lineReuseLength)) {
      contents(range.start.line + newLineIndex) = newLines(newLineIndex)
    }
    val diff = newLines.length - rangeLineDifference
    val currentLine = range.start.line + lineReuseLength
    if (diff > 0) {
      contents.insertAll(currentLine, newLines.drop(lineReuseLength))
    } else {
      contents.remove(currentLine, -diff)
    }
  }

  def mkString = {
    contents.map(line => new String(line)).mkString(newLine)
  }
}
