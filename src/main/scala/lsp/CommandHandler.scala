package lsp

import langserver.messages._

trait CommandHandler {
  def handle(method: String, command: ServerCommand): Any
}


