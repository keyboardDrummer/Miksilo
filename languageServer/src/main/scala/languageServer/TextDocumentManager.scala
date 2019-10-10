package languageServer

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.util.concurrent.{ConcurrentHashMap, ConcurrentMap}

import com.typesafe.scalalogging.LazyLogging
import core.language.FileSystem

import scala.collection.JavaConverters._
import scala.tools.nsc.interpreter.InputStream

/**
  * A class to manage text documents coming over the wire from a Language Server client.
  *
  * The manager keeps an up to date version of each document that is currently open by the client.
  */
class TextDocumentManager extends LazyLogging with FileSystem {

  private val docs: ConcurrentMap[String, InMemoryTextDocument] = new ConcurrentHashMap

  def getOpenDocumentForUri(uri: String): Option[InMemoryTextDocument] =
    Option(docs.get(uri))

  def allOpenDocuments: Seq[InMemoryTextDocument] = docs.values.asScala.toSeq

  def onOpenTextDocument(testDocument: TextDocumentItem): InMemoryTextDocument = {
    docs.put(testDocument.uri, InMemoryTextDocument(testDocument.uri, testDocument.text.toCharArray))
  }

  def onChangeTextDocument(documentIdentifier: VersionedTextDocumentIdentifier, changes: Seq[TextDocumentContentChangeEvent]): InMemoryTextDocument = {
    docs.get(documentIdentifier.uri) match {
      case null =>
        logger.error(s"Document ${documentIdentifier.uri} not found in this manager. Adding now")
        // we assume full text sync
        docs.put(documentIdentifier.uri, InMemoryTextDocument(documentIdentifier.uri, changes.head.text.toCharArray))
      case doc =>
        docs.put(documentIdentifier.uri, doc.applyChanges(changes))
    }
  }

  def onCloseTextDocument(td: TextDocumentIdentifier): InMemoryTextDocument = {
    docs.remove(td.uri)
  }

  override def getFile(path: String): InputStream = {
    val bytes = getOpenDocumentForUri(path).get.contents
    stringToStream(new String(bytes)) //TODO maybe instead of Input stream een byte array gebruiken?
  }

  // TODO remove this method and let FileSystem stop using InputStream
  def stringToStream(input: String) = new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8))
}

import java.io.File
import java.net.URI

case class InMemoryTextDocument(uri: String, contents: Array[Char]) {
  def applyChanges(changes: Seq[TextDocumentContentChangeEvent]): InMemoryTextDocument = {
    // we assume full text sync
    assert(changes.size == 1)
    val change = changes.head
    assert(change.range.isEmpty)
    assert(change.rangeLength.isEmpty)

    copy(contents = change.text.toArray)
  }

  private def peek(idx: Int) =
    if (idx < contents.size) contents(idx) else -1

  def toFile: File =
    new File(URI.create(uri))

  /**
    * Return the corresponding position in this text document as 0-based line and column.
    */
  def offsetToPosition(offset: Int): Position = {
    if (offset >= contents.size)
      throw new IndexOutOfBoundsException(s"$uri: asked position at offset $offset, but contents is only ${contents.size} characters long.")

    var i, line, col = 0

    while (i < offset) {
      contents(i) match {
        case '\r' =>
          line += 1
          col = 0
          if (peek(i + 1) == '\n') i += 1

        case '\n' =>
          line += 1
          col = 0

        case _ =>
          col += 1
      }
      i += 1
    }

    Position(line, col)
  }

  /**
    * Return the offset in the current document, for a given 0-based line/col position.
    */
  def positionToOffset(pos: Position): Int = {
    val Position(line, col) = pos

    var i, l, c = 0
    while (i < contents.size && l < line) {
      contents(i) match {
        case '\r' =>
          l += 1
          if (peek(i + 1) == '\n') i += 1

        case '\n' =>
          l += 1

        case _ =>
      }
      i += 1
    }

    if (l < line)
      throw new IllegalArgumentException(s"$uri: Can't find position $pos in contents of only $l lines long.")
    if (i + col < contents.size)
      i + col
    else
      throw new IllegalArgumentException(s"$uri: Invalid column. Position $pos in line '${contents.slice(i, contents.size).mkString}'")
  }

  def lineToOffset(lineNr: Int): Int = {
    positionToOffset(Position(lineNr, 0))
  }

}