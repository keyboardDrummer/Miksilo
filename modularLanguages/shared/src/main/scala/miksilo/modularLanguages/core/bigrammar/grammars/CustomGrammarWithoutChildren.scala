package miksilo.modularLanguages.core.bigrammar.grammars

import miksilo.modularLanguages.core.bigrammar.{BiGrammar, BiGrammarToParser}
import miksilo.modularLanguages.core.bigrammar.printer.Printer.NodePrinter

trait CustomGrammarWithoutChildren extends BiGrammar with NodePrinter {
  def getParserBuilder(keywords: scala.collection.Set[String]): BiGrammarToParser.Parser[Any]
}
