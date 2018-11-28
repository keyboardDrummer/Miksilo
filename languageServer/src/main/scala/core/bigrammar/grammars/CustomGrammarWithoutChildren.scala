package core.bigrammar.grammars

import core.bigrammar.printer.Printer.NodePrinter
import core.bigrammar.{BiGrammar, BiGrammarToParser}

trait CustomGrammarWithoutChildren extends BiGrammar with NodePrinter {
  def getParser(keywords: scala.collection.Set[String]): BiGrammarToParser.Processor[Any]
}
