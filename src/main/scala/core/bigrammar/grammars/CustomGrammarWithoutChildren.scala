package core.bigrammar.grammars

import core.bigrammar.BiGrammar
import core.bigrammar.printer.Printer.NodePrinter
import core.grammar.Grammar

trait CustomGrammarWithoutChildren extends BiGrammar with NodePrinter {
  def getGrammar: Grammar
}
