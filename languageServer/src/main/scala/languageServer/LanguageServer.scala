package languageServer

import core.language.node.FileRange
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
  def gotoDefinition(parameters: DocumentPosition): Seq[FileRange]
}

case class ReferencesParams(textDocument: TextDocumentIdentifier, position: Position, context: ReferenceContext)

trait ReferencesProvider {
  def references(location: ReferencesParams): Seq[FileRange]
}

trait LanguageClient {
  def sendDiagnostics(diagnostics: PublishDiagnostics)
}

trait LanguageServer {
  def setClient(client: LanguageClient): Unit

  def initialize(parameters: InitializeParams): Unit
  def didOpen(parameters: TextDocumentItem): Unit
  def didClose(parameters: TextDocumentIdentifier): Unit
  def didSave(parameters: DidSaveTextDocumentParams): Unit
  def didChange(parameters: DidChangeTextDocumentParams): Unit
  def initialized(): Unit
}