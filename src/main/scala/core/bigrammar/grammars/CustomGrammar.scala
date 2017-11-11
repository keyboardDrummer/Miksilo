package core.bigrammar.grammars

import core.bigrammar.BiGrammar
import core.bigrammar.printer.TryState.NodePrinter
import core.grammar.Grammar

trait CustomGrammar extends BiGrammar with NodePrinter {
  def getGrammar: Grammar
}
