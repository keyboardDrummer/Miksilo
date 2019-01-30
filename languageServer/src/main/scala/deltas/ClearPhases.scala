package deltas

import core.deltas.{Contract, Delta}
import core.language.Language

object ClearPhases extends Delta {
  override def description: String = "Removes all defined phases"

  override def inject(language: Language): Unit = {
    language.compilerPhases.clear()
  }

  override def dependencies: Set[Contract] = Set.empty
}
