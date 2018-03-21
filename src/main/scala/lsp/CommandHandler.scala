package lsp

trait CommandHandler {
  def handle(method: String, command: ServerCommand): Any
}


