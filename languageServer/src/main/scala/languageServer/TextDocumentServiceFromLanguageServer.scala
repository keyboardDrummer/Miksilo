package languageServer

import org.eclipse.lsp4j
import org.eclipse.lsp4j.{DidCloseTextDocumentParams, DidOpenTextDocumentParams}

import scala.collection.JavaConverters

class TextDocumentServiceFromLanguageServer(languageServer: LanguageServer)
  extends org.eclipse.lsp4j.services.TextDocumentService {

  import Conversions._

  override def didOpen(params: DidOpenTextDocumentParams): Unit = {
    val document = params.getTextDocument
    languageServer.didOpen(TextDocumentItem(document.getUri, document.getLanguageId, document.getVersion, document.getText))
  }

  override def didChange(params: lsp4j.DidChangeTextDocumentParams): Unit = {
    val documentIdentifier = VersionedTextDocumentIdentifier(params.getTextDocument.getUri, params.getTextDocument.getVersion)
    val changes = JavaConverters.asScalaBuffer(params.getContentChanges).map(
      e => TextDocumentContentChangeEvent(Option(e.getRange), Option(e.getRangeLength), e.getText))

    languageServer.didChange(DidChangeTextDocumentParams(documentIdentifier, changes))
  }

  override def didClose(params: DidCloseTextDocumentParams): Unit = {
    languageServer.didClose(TextDocumentIdentifier(params.getTextDocument.getUri))
  }

  override def didSave(params: lsp4j.DidSaveTextDocumentParams): Unit = {
    languageServer.didClose(TextDocumentIdentifier(params.getTextDocument.getUri))
  }
}
