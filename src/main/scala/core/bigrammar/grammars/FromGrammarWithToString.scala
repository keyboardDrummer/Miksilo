package core.bigrammar.grammars

import core.bigrammar.BiGrammarToGrammar.WithMap
import core.bigrammar.WithMapG
import core.bigrammar.printer.TryState.State
import core.grammar.Grammar

class FromGrammarWithToString(grammar: Grammar, verifyWhenPrinting: Boolean = true)
  extends FromStringGrammar(grammar, verifyWhenPrinting) {

  override def write(from: WithMap, state: State) =
    super.write(WithMapG(from.value.toString, from.map), state)
}
