package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser.WithMap
import core.bigrammar.WithMapG
import core.bigrammar.printer.TryState
import core.grammar.Grammar
import core.responsiveDocument.ResponsiveDocument

class FromGrammarWithToString(grammar: Grammar, verifyWhenPrinting: Boolean = true)
  extends FromStringGrammar(grammar, verifyWhenPrinting) {

  override def write(from: WithMap): TryState[ResponsiveDocument] =
    super.write(WithMapG(from.value.toString, from.map))
}
