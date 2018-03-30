package languageServer.lsp

object LSPProtocol {
  val definition = "textDocument/definition"
  val completion = "textDocument/completion"
  val hover = "textDocument/hover"
  val didOpen = "textDocument/didOpen"
  val didClose = "textDocument/didClose"
  val didChange = "textDocument/didChange"
  val didSave = "textDocument/didSave"
  val initialized = "initialized"
  val initialize = "initialize"
}
