package core.bigrammar.grammars

import core.bigrammar.printer.Printer.NodePrinter
import core.bigrammar.{BiGrammar, BiGrammarToParser}

trait CustomGrammarWithoutChildren extends BiGrammar with NodePrinter {
  def getParser: BiGrammarToParser.Parser[Any]
}
