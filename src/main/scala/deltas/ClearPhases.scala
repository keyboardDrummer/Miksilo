package deltas

import core.deltas.Delta
import core.language.Language

object ClearPhases extends Delta {
  override def description: String = "Removes all defined phases"

  override def inject(language: Language): Unit = {
    language.compilerPhases = List.empty
  }
}
