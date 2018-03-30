package languageServer

import langserver.types._
import languageServer.lsp._

trait DocumentSymbolProvider {
  def documentSymbols(tdi: TextDocumentIdentifier): Seq[SymbolInformation]
}

trait HoverProvider {
  def hoverRequest(request: TextDocumentHoverRequest): Hover
}

trait CompletionProvider {
  def getOptions: CompletionOptions
  def complete(request: DocumentPosition): CompletionList
}

trait DefinitionProvider {
  def gotoDefinition(parameters: DocumentPosition): Seq[Location]
}

trait LanguageServer {
  def initialize(parameters: InitializeParams): Unit
  def didOpen(parameters: TextDocumentItem): Unit
  def didClose(parameters: TextDocumentIdentifier): Unit
  def didSave(parameters: TextDocumentIdentifier): Unit
  def didChange(parameters: DidChangeTextDocumentParams): Unit
  def initialized(): Unit
}