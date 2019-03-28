package languageServer.lsp

object LSPProtocol {
  val diagnostics = "textDocument/publishDiagnostics"
  val references = "textDocument/references"
  val definition = "textDocument/definition"
  val documentSymbol = "textDocument/documentSymbol"
  val completion = "textDocument/completion"
  val hover = "textDocument/hover"
  val didOpen = "textDocument/didOpen"
  val didClose = "textDocument/didClose"
  val didChange = "textDocument/didChange"
  val didSave = "textDocument/didSave"
  val initialized = "initialized"
  val initialize = "initialize"
}
