package miksilo.modularLanguages.deltas

import miksilo.modularLanguages.core.deltas.{Contract, Delta}
import miksilo.languageServer.core.language.Language

object ClearPhases extends Delta {
  override def description: String = "Removes all defined phases"

  override def inject(language: Language): Unit = {
    language.compilerPhases = List.empty
  }

  override def dependencies: Set[Contract] = Set.empty
}
