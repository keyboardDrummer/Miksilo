package languageServer.lsp

import com.dhpcs.jsonrpc.JsonRpcMessage.{CorrelationId, NumericCorrelationId}
import langserver.types.{Location, TextDocumentIdentifier, TextDocumentItem}
import play.api.libs.json.{Json, Reads}

import scala.concurrent.Promise

class LSPClient(connection: JsonRpcConnection) {

  val simpleConnection = new SimpleJsonRpcHandler(connection)
  var correlationId = 0

  def listen(): Unit = {
    connection.listen()
  }

  def getCorrelationId: CorrelationId = {
    val result = correlationId
    correlationId += 1
    NumericCorrelationId(result)
  }

  def gotoDefinition(parameters: DocumentPosition): Promise[Seq[Location]] = {
    simpleConnection.sendRequest[DocumentPosition, Seq[Location]](
      LSPProtocol.definition, getCorrelationId, parameters)(Json.format, Reads.of[Seq[Location]])
  }

  def complete(parameters: DocumentPosition): Promise[CompletionList] = {
    simpleConnection.sendRequest[DocumentPosition, CompletionList](
      LSPProtocol.completion, getCorrelationId, parameters)(Json.format, Json.format)
  }

  def initialize(parameters: InitializeParams): Promise[InitializeResult] = {
    simpleConnection.sendRequest[InitializeParams, InitializeResult](
      LSPProtocol.initialize, getCorrelationId, parameters)(Json.format, Json.format)
  }

  def didOpen(parameters: TextDocumentItem): Unit = {
    simpleConnection.sendNotification[TextDocumentItem](LSPProtocol.didOpen, parameters)(Json.format)
  }

  def didClose(identifier: TextDocumentIdentifier): Unit = {
    simpleConnection.sendNotification[TextDocumentIdentifier](LSPProtocol.didOpen, identifier)(Json.format)
  }

  def didSave(identifier: TextDocumentIdentifier): Unit = {
    simpleConnection.sendNotification[TextDocumentIdentifier](LSPProtocol.didSave, identifier)(Json.format)
  }

  def didChange(parameters: DidChangeTextDocumentParams): Unit = {
    simpleConnection.sendNotification[DidChangeTextDocumentParams](LSPProtocol.didChange, parameters)(Json.format)
  }

  def initialized(): Unit = {
    ??? //simpleConnection.sendNotification[DidChangeTextDocumentParams](LSPProtocol.initialized, parameters)(Json.format)
  }
}
