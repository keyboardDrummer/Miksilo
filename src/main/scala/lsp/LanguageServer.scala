package lsp

import com.typesafe.scalalogging.Logger
import langserver.messages.MessageType
import langserver.types._
import org.slf4j.LoggerFactory

/**
  * A language server implementation. Users should subclass this class and implement specific behavior.
  */
class LanguageServer(connection: Connection) {

  lazy val logger: Logger = Logger(LoggerFactory.getLogger(getClass.getName))

  protected val documentManager = new TextDocumentManager(connection)

  connection.notificationHandlers += {
    case DidOpenTextDocumentParams(td) => onOpenTextDocument(td)
    case DidChangeTextDocumentParams(td, changes) => onChangeTextDocument(td, changes)
    case DidSaveTextDocumentParams(td) => onSaveTextDocument(td)
    case DidCloseTextDocumentParams(td) => onCloseTextDocument(td)
    case DidChangeWatchedFiles(changes) => onChangeWatchedFiles(changes)
    case _:Initialized =>
    case e => logger.error(s"Unknown notification $e")
  }
  connection.setServer(this)

  def onOpenTextDocument(td: TextDocumentItem) = {
    logger.debug(s"openTextDocuemnt $td")
  }

  def onChangeTextDocument(td: VersionedTextDocumentIdentifier, changes: Seq[TextDocumentContentChangeEvent]) = {
    logger.debug(s"changeTextDocuemnt $td")
  }

  def onSaveTextDocument(td: TextDocumentIdentifier) = {
    logger.debug(s"saveTextDocuemnt $td")
    connection.showMessage(MessageType.Info, s"Saved text document ${td.uri}")
  }

  def onCloseTextDocument(td: TextDocumentIdentifier) = {
    logger.debug(s"closeTextDocuemnt $td")
  }

  def onChangeWatchedFiles(changes: Seq[FileEvent]) = {
  }

  def initialize(pid: Option[Long], rootPath: String, capabilities: ClientCapabilities): ServerCapabilities = {
    ServerCapabilities(
      documentSymbolProvider = this.isInstanceOf[DocumentSymbolProvider],
      hoverProvider = this.isInstanceOf[HoverProvider],
      definitionProvider = this.isInstanceOf[GotoProvider],
      completionProvider = this match {
        case provider: CompletionProvider => Some(provider.getOptions)
        case _ => None
      })
  }

  def shutdown(): Unit = {

  }
}

trait DocumentSymbolProvider {
  def documentSymbols(tdi: TextDocumentIdentifier): Seq[SymbolInformation]
}

trait HoverProvider {
  def hoverRequest(textDocument: TextDocumentIdentifier, position: Position): Hover
}

trait CompletionProvider {
  def getOptions: CompletionOptions
  def completionRequest(textDocument: TextDocumentIdentifier, position: Position): CompletionList
}

trait GotoProvider {
  def gotoDefinitionRequest(textDocument: TextDocumentIdentifier, position: Position): DefinitionResult
}