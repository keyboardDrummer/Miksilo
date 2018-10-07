package languageServer

import java.util.concurrent.{ConcurrentHashMap, ConcurrentMap}

import com.typesafe.scalalogging.LazyLogging
import core.language.FileSystem
import langserver.core.TextDocument
import langserver.types.{TextDocumentContentChangeEvent, TextDocumentIdentifier, TextDocumentItem, VersionedTextDocumentIdentifier}
import util.SourceUtils

import scala.collection.JavaConverters._
import scala.tools.nsc.interpreter.InputStream

/**
  * A class to manage text documents coming over the wire from a Language Server client.
  *
  * The manager keeps an up to date version of each document that is currently open by the client.
  */
class TextDocumentManager extends LazyLogging with FileSystem {

  private val docs: ConcurrentMap[String, TextDocument] = new ConcurrentHashMap

  def getOpenDocumentForUri(uri: String): Option[TextDocument] =
    Option(docs.get(uri))

  def allOpenDocuments: Seq[TextDocument] = docs.values.asScala.toSeq

  def onOpenTextDocument(testDocument: TextDocumentItem): TextDocument = {
    docs.put(testDocument.uri, TextDocument(testDocument.uri, testDocument.text.toCharArray))
  }

  def onChangeTextDocument(documentIdentifier: VersionedTextDocumentIdentifier, changes: Seq[TextDocumentContentChangeEvent]): TextDocument = {
    docs.get(documentIdentifier.uri) match {
      case null =>
        logger.error(s"Document ${documentIdentifier.uri} not found in this manager. Adding now")
        // we assume full text sync
        docs.put(documentIdentifier.uri, TextDocument(documentIdentifier.uri, changes.head.text.toCharArray))
      case doc =>
        docs.put(documentIdentifier.uri, doc.applyChanges(changes))
    }
  }

  def onCloseTextDocument(td: TextDocumentIdentifier): TextDocument = {
    docs.remove(td.uri)
  }

  override def getFile(path: String): InputStream = {
    val bytes = getOpenDocumentForUri(path).get.contents
    SourceUtils.stringToStream(new String(bytes)) //TODO maybe instead of Input stream een byte array gebruiken?
  }
}
