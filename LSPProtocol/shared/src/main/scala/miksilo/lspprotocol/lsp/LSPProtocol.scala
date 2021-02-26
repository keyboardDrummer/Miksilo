package miksilo.lspprotocol.lsp

object LSPProtocol {
  val typeDefinition = "textDocument/typeDefinition"
  val telemetry = "telemetry/event"
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
  val rename = "textDocument/rename"
  val initialized = "initialized"
  val initialize = "initialize"
  val shutdown = "shutdown"
  val exit = "exit"
  val codeAction = "textDocument/codeAction"
}
