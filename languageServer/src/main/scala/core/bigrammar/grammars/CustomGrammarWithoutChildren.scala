package core.bigrammar.grammars

import core.bigrammar.printer.Printer.NodePrinter
import core.bigrammar.{BiGrammar, BiGrammarToParser}

trait CustomGrammarWithoutChildren extends BiGrammar with NodePrinter {
  def getParserBuilder(keywords: scala.collection.Set[String]): BiGrammarToParser.Self[Any]
}
