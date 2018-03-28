package languageServer.lsp

import com.dhpcs.jsonrpc.JsonRpcMessage.{CorrelationId, NumericCorrelationId}
import langserver.types.TextDocumentIdentifier
import play.api.libs.json.Json

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

  def gotoDefinition(parameters: DocumentPosition): Promise[DefinitionResult] = {
    simpleConnection.sendRequest[DocumentPosition, DefinitionResult](
      LSPProtocol.definition, getCorrelationId, parameters)(Json.format, Json.format)
  }

  def initialize(parameters: InitializeParams): Promise[InitializeResult] = {
    simpleConnection.sendRequest[InitializeParams, InitializeResult](
      LSPProtocol.initialize, getCorrelationId, parameters)(Json.format, Json.format)
  }

  def didOpen(parameters: DidOpenTextDocumentParams): Unit = {
    simpleConnection.sendNotification[DidOpenTextDocumentParams](LSPProtocol.didOpen, parameters)(Json.format)
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
