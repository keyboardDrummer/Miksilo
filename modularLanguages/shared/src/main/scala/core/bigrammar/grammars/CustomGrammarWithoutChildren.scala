package core.bigrammar.grammars

import core.bigrammar.{BiGrammar, BiGrammarToParser}
import core.bigrammar.printer.Printer.NodePrinter

trait CustomGrammarWithoutChildren extends BiGrammar with NodePrinter {
  def getParserBuilder(keywords: scala.collection.Set[String]): BiGrammarToParser.Parser[Any]
}
