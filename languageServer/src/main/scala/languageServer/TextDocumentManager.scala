package languageServer

import java.io.{ByteArrayInputStream, InputStream}
import java.nio.charset.StandardCharsets
import java.util.concurrent.{ConcurrentHashMap, ConcurrentMap}
import com.typesafe.scalalogging.LazyLogging
import core.language.FileSystem
import lsp.{TextDocumentContentChangeEvent, TextDocumentIdentifier, TextDocumentItem, VersionedTextDocumentIdentifier}

import scala.collection.JavaConverters.collectionAsScalaIterable

/**
  * A class to manage text documents coming over the wire from a Language Server client.
  *
  * The manager keeps an up to date version of each document that is currently open by the client.
  */
class TextDocumentManager extends LazyLogging with FileSystem {

  private val docs: ConcurrentMap[String, InMemoryTextDocument] = new ConcurrentHashMap

  def getOpenDocumentForUri(uri: String): Option[InMemoryTextDocument] =
    Option(docs.get(uri))

  def allOpenDocuments: Seq[InMemoryTextDocument] = collectionAsScalaIterable(docs.values).toSeq

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
        docs.get(documentIdentifier.uri).applyUnsafeChanges(changes)
    }
  }

  def onCloseTextDocument(td: TextDocumentIdentifier): InMemoryTextDocument = {
    docs.remove(td.uri)
  }

  override def getFile(path: String): InputStream = {
    val bytes: Array[Byte] = getOpenDocumentForUri(path).get.mkString.getBytes("UTF-8")
    new ByteArrayInputStream(bytes) //TODO maybe instead of Input stream een byte array gebruiken?
  }

  // TODO remove this method and let FileSystem stop using InputStream
  def stringToStream(input: String) = new ByteArrayInputStream(input.getBytes(StandardCharsets.UTF_8))
}

