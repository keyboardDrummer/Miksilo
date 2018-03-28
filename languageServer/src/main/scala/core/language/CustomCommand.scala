package core.language

import core.deltas.Delta
import languageServer.LanguageServer

trait CustomCommand {
  def name: String

  def initialize(deltas: Seq[Delta])
  def perform(server: LanguageServer)
}
