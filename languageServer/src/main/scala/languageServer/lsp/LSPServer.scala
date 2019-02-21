package languageServer.lsp

import core.language.node.FileRange
import langserver.types.{Location, ReferenceContext}
import languageServer._
import play.api.libs.json._

import scala.reflect.ClassTag

class LSPServer(languageServer: LanguageServer, connection: JsonRpcConnection) {

  val handler = new SimpleJsonRpcHandler(connection)
  addRequestHandlers()
  addNotificationHandlers()
  languageServer.setClient(new LanguageClientProxy())

  class LanguageClientProxy extends LanguageClient {
    override def sendDiagnostics(diagnostics: PublishDiagnostics): Unit = {
      implicit val publishDiagnosticsFormat: OFormat[PublishDiagnostics] = Json.format
      handler.sendNotification(LSPProtocol.diagnostics, diagnostics)
    }
  }

  def listen(): Unit = {
    connection.listen()
  }

  def addRequestHandlers(): Unit = {

    handler.addRequestHandler[InitializeParams, InitializeResult](LSPProtocol.initialize, initialize)(Json.format, Json.format)

    def addProvider[Provider: ClassTag, Request, Response](method: String, getHandler: Provider => Request => Response)
                                                          (requestFormat: Reads[Request], responseFormat: Writes[Response]): Unit = {
      languageServer match {
        case provider: Provider =>
          handler.addRequestHandler(method, getHandler(provider))(requestFormat, responseFormat)
        case _ =>
      }
    }

    implicit val textDocumentPositionParams: OFormat[DocumentPosition] = Json.format
    implicit val referenceContext: OFormat[ReferenceContext] = Json.format

    addProvider(LSPProtocol.definition, (provider: DefinitionProvider) => (position: DocumentPosition) => {
      val fileRanges = provider.gotoDefinition(position)
      fileRanges.map(fileRangeToLocation)
    })(Json.format, Writes.of[Seq[Location]])

    addProvider(LSPProtocol.references, (provider: ReferencesProvider) => (parameters: ReferencesParams) => {
      val fileRanges = provider.references(parameters)
      fileRanges.map(fileRangeToLocation)
    })(Json.format, Writes.of[Seq[Location]])

    addProvider(LSPProtocol.completion, (provider: CompletionProvider) => provider.complete)(Json.format, Json.format)
    addProvider(LSPProtocol.hover, (provider: HoverProvider) => provider.hoverRequest)(Json.format[TextDocumentHoverRequest], Json.format[Hover])
  }

  def fileRangeToLocation(fileRange: FileRange): Location =
    new Location(fileRange.uri, new langserver.types.Range(fileRange.range.start, fileRange.range.end))

  def addNotificationHandlers(): Unit = {
    handler.addNotificationHandler(LSPProtocol.didOpen, (params: DidOpenTextDocumentParams) => languageServer.didOpen(params.textDocument))(Json.format)
    handler.addNotificationHandler(LSPProtocol.didChange, languageServer.didChange)(Json.format)
    handler.addNotificationHandler(LSPProtocol.didClose, (params: DidCloseTextDocumentParams) => languageServer.didClose(params.identifier))(Json.format)
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
      referencesProvider = languageServer.isInstanceOf[ReferencesProvider],
      hoverProvider = languageServer.isInstanceOf[HoverProvider],
      definitionProvider = languageServer.isInstanceOf[DefinitionProvider],
      completionProvider = languageServer match {
        case provider: CompletionProvider => Some(provider.getOptions)
        case _ => None
      })
  }
}
