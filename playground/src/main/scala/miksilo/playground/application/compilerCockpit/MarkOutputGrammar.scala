package playground.application.compilerCockpit

import core.deltas.{Contract, Delta}
import miksilo.languageServer.core.language.Language

object MarkOutputGrammar extends Delta {
  override def name = "pretty print"
  override def description: String = "Use as an anchor to specify where the output grammar is defined."

  override def dependencies: Set[Contract] = Set.empty

  override def inject(language: Language): Unit = {}
}
