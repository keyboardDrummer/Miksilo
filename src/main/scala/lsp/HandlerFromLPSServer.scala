package lsp

import langserver.messages._

class HandlerFromLPSServer(server: LanguageServer) extends CommandHandler {
  override def handle(method: String, command: ServerCommand): Any = {
      (method, command) match {
        case (_, InitializeParams(pid, rootPath, capabilities)) =>
          InitializeResult(server.initialize(pid, rootPath, capabilities))
        case ("textDocument/completion", TextDocumentCompletionRequest(TextDocumentPositionParams(textDocument, position))) =>
          server.completionRequest(textDocument, position)
        case ("textDocument/definition", TextDocumentDefinitionRequest(TextDocumentPositionParams(textDocument, position))) =>
          server.gotoDefinitionRequest(textDocument, position)
        case ("textDocument/hover", TextDocumentHoverRequest(TextDocumentPositionParams(textDocument, position))) =>
          server.hoverRequest(textDocument, position)
        case ("textDocument/documentSymbol", DocumentSymbolParams(tdi)) =>
          DocumentSymbolResult(server.documentSymbols(tdi))

        case (_, Shutdown()) =>
          server.shutdown()
          ShutdownResult(0) // the value is a dummy, because Play Json needs to serialize something
        case c =>
          server.logger.error(s"Unknown command $c")
          sys.error("Unknown command")
      }
  }
}
