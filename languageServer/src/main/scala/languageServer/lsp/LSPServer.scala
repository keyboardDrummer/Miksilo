package languageServer.lsp

import languageServer._
import play.api.libs.json.{Json, OFormat}

import scala.reflect.ClassTag

class LSPServer(languageServer: LanguageServer, connection: JsonRpcConnection) {

  val handler = new SimpleJsonRpcHandler(connection)
  addRequestHandlers()

  def listen(): Unit = {
    connection.listen()
  }

  def addRequestHandlers(): Unit = {

    handler.addRequestHandler[InitializeParams, InitializeResult](LSPProtocol.initialize, initialize)(Json.format, Json.format)

    def addProvider[Provider: ClassTag, Request, Response](method: String, getHandler: Provider => (Request => Response))
                                                          (requestFormat: OFormat[Request], responseFormat: OFormat[Response]): Unit = {
      languageServer match {
        case provider: Provider =>
          handler.addRequestHandler(method, getHandler(provider))(requestFormat, responseFormat)
        case _ =>
      }
    }

    implicit val textDocumentPositionParams: OFormat[DocumentPosition] = Json.format
    addProvider(LSPProtocol.definition, (provider: DefinitionProvider) => provider.gotoDefinition)(Json.format, Json.format[DefinitionResult])
    addProvider(LSPProtocol.completion, (provider: CompletionProvider) => provider.complete)(Json.format, Json.format)
    addProvider(LSPProtocol.hover, (provider: HoverProvider) => provider.hoverRequest)(Json.format[TextDocumentHoverRequest], Json.format[Hover])
  }

  def addNotificationHandlers(): Unit = {
    handler.addNotificationHandler(LSPProtocol.didOpen, languageServer.didOpen)(Json.format)
    handler.addNotificationHandler(LSPProtocol.didChange, languageServer.didChange)(Json.format)
    handler.addNotificationHandler(LSPProtocol.didClose, languageServer.didClose)(Json.format)
    handler.addNotificationHandler(LSPProtocol.didSave, languageServer.didSave)(Json.format)

    handler.getNotificationHandlersForMethod(LSPProtocol.initialized).append(_ => languageServer.initialized())
  }

  def initialize(parameters: InitializeParams): InitializeResult = {
    languageServer.initialize(parameters)
    InitializeResult(getCapabilities)
  }

  def getCapabilities: ServerCapabilities = {
    ServerCapabilities(
      documentSymbolProvider = languageServer.isInstanceOf[DocumentSymbolProvider],
      hoverProvider = languageServer.isInstanceOf[HoverProvider],
      definitionProvider = languageServer.isInstanceOf[DefinitionProvider],
      completionProvider = languageServer match {
        case provider: CompletionProvider => Some(provider.getOptions)
        case _ => None
      })
  }
}
