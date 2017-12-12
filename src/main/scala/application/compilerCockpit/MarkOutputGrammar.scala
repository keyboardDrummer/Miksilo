package application.compilerCockpit

import core.deltas.{Contract, Delta}

object MarkOutputGrammar extends Delta {
  override def name = "pretty print"
  override def description: String = "Use as an anchor to specify where the output grammar is defined."

  override def dependencies: Set[Contract] = Set.empty
}
