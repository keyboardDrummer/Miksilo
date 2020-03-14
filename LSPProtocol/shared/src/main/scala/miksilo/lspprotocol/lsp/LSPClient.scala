package miksilo.lspprotocol.lsp

import com.dhpcs.jsonrpc.JsonRpcMessage.{CorrelationId, NumericCorrelationId}
import miksilo.lspprotocol.jsonRpc.{JsonRpcConnection, MethodBasedJsonRpcHandler}
import play.api.libs.json.{Json, Reads}
import miksilo.lspprotocol.lsp._
import scala.concurrent.Future

class LSPClient(languageClient: LanguageClient, connection: JsonRpcConnection) {

  val simpleConnection = new MethodBasedJsonRpcHandler(connection)
  connection.setHandler(simpleConnection)

  var correlationId = 0

  simpleConnection.addNotificationHandler[PublishDiagnostics](LSPProtocol.diagnostics, notification => {
    languageClient.sendDiagnostics(notification)
  })(Json.format)

  def listen(): Unit = {
    connection.listen()
  }

  def getCorrelationId: CorrelationId = {
    val result = correlationId
    correlationId += 1
    NumericCorrelationId(result)
  }

  implicit val positionFormat = PositionFormat.format
  implicit val sourceRangeFormat = SourceRangeFormat.format
  implicit val textEditFormat = TextEditFormat.format
  def rename(parameters: RenameParams): Future[WorkspaceEdit] = {
    simpleConnection.sendRequest[RenameParams, WorkspaceEdit](
      LSPProtocol.rename, getCorrelationId, parameters)(Json.format, Json.format[WorkspaceEdit])
  }

  def documentSymbol(parameters: DocumentSymbolParams): Future[Seq[SymbolInformation]] = {
    simpleConnection.sendRequest[DocumentSymbolParams, Seq[SymbolInformation]](
      LSPProtocol.documentSymbol, getCorrelationId, parameters)(Json.format, Reads.of[Seq[SymbolInformation]])
  }

  def references(parameters: ReferencesParams): Future[Seq[FileRange]] = {
    simpleConnection.sendRequest[ReferencesParams, Seq[FileRange]](
      LSPProtocol.references, getCorrelationId, parameters)(Json.format, Reads.of[Seq[FileRange]])
  }

  def codeAction(parameters: CodeActionParams): Future[Seq[CodeAction]] = {
    simpleConnection.sendRequest[CodeActionParams, Seq[CodeAction]](
      LSPProtocol.codeAction, getCorrelationId, parameters)(Json.format, Reads.of[Seq[CodeAction]])
  }

  def gotoDefinition(parameters: DocumentPosition): Future[Seq[FileRange]] = {
    simpleConnection.sendRequest[DocumentPosition, Seq[FileRange]](
      LSPProtocol.definition, getCorrelationId, parameters)(Json.format, Reads.of[Seq[FileRange]])
  }

  def complete(parameters: DocumentPosition): Future[CompletionList] = {
    simpleConnection.sendRequest[DocumentPosition, CompletionList](
      LSPProtocol.completion, getCorrelationId, parameters)(Json.format, Json.format)
  }

  def initialize(parameters: InitializeParams): Future[InitializeResult] = {
    simpleConnection.sendRequest[InitializeParams, InitializeResult](
      LSPProtocol.initialize, getCorrelationId, parameters)(Json.format, Json.format)
  }

  def didOpen(parameters: TextDocumentItem): Unit = {
    simpleConnection.sendNotification[DidOpenTextDocumentParams](LSPProtocol.didOpen, DidOpenTextDocumentParams(parameters))(Json.format)
  }

  def didClose(identifier: TextDocumentIdentifier): Unit = {
    simpleConnection.sendNotification[DidCloseTextDocumentParams](LSPProtocol.didClose, DidCloseTextDocumentParams(identifier))(Json.format)
  }

  def didSave(params: DidSaveTextDocumentParams): Unit = {
    simpleConnection.sendNotification[DidSaveTextDocumentParams](LSPProtocol.didSave, params)(Json.format)
  }

  def didChange(parameters: DidChangeTextDocumentParams): Unit = {
    simpleConnection.sendNotification[DidChangeTextDocumentParams](LSPProtocol.didChange, parameters)(Json.format)
  }

  def initialized(): Unit = {
    ??? //simpleConnection.sendNotification[DidChangeTextDocumentParams](LSPProtocol.initialized, parameters)(Json.format)
  }
}
