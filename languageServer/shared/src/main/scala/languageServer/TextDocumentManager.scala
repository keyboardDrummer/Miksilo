package languageServer

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.util.concurrent.{ConcurrentHashMap, ConcurrentMap}

import core.LazyLogging
import core.language.{FileSystem, DocumentEventListener}
import lsp.{TextDocumentContentChangeEvent, TextDocumentIdentifier, TextDocumentItem, VersionedTextDocumentIdentifier}

import scala.collection.mutable
import scala.jdk.CollectionConverters

/**
  * A class to manage text documents coming over the wire from a Language Server client.
  *
  * The manager keeps an up to date version of each document that is currently open by the client.
  */
class TextDocumentManager extends LazyLogging with FileSystem {

  private val docs: ConcurrentMap[String, InMemoryTextDocument] = new ConcurrentHashMap
  private val handlers: mutable.HashMap[String, DocumentEventListener] = new mutable.HashMap

  def getOpenDocumentForUri(uri: String): Option[InMemoryTextDocument] =
    Option(docs.get(uri))

  def allOpenDocuments: Seq[InMemoryTextDocument] = CollectionConverters.CollectionHasAsScala(docs.values).asScala.toSeq

  def onOpenTextDocument(testDocument: TextDocumentItem): InMemoryTextDocument = {
    docs.put(testDocument.uri, new InMemoryTextDocument(testDocument.uri, testDocument.text))
  }

  def onChangeTextDocument(documentIdentifier: VersionedTextDocumentIdentifier, changes: Seq[TextDocumentContentChangeEvent]): Unit = {
    docs.get(documentIdentifier.uri) match {
      case null =>
        logger.error(s"Document ${documentIdentifier.uri} not found in this manager. Adding now")
        // we assume full text sync
        docs.put(documentIdentifier.uri, new InMemoryTextDocument(documentIdentifier.uri, changes.head.text))
      case doc =>
        docs.get(documentIdentifier.uri).applyUnsafeChanges(changes, handlers.get(documentIdentifier.uri))
    }
  }

  def onCloseTextDocument(td: TextDocumentIdentifier): InMemoryTextDocument = {
    val result = docs.remove(td.uri)
    handlers.get(td.uri).foreach(h => h.handleClose())
    result
  }

  override def getFile(path: String): String = {
    getOpenDocumentForUri(path).get.mkString
  }

  // TODO remove this method and let FileSystem stop using InputStream
  def stringToStream(input: String) = new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8))

  override def setDocumentEventListener(uri: String, handler: DocumentEventListener): Unit = {
    if (handler == null) {
      handlers.remove(uri)
      return
    }

    if (handlers.contains(uri)) {
      throw new Exception("uri already had a handler")
    }
    handlers.put(uri, handler)
  }

  override def getFileParseText(path: String) = docs.get(path).parseText
}

