package core.language

import core.deltas.Delta
import lsp.LanguageServer

trait CustomCommand {
  def name: String

  def initialize(deltas: Seq[Delta])
  def perform(server: LanguageServer)
}
