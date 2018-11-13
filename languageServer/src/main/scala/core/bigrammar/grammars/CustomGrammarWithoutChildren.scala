package core.bigrammar.grammars

import core.bigrammar.BiGrammar
import core.bigrammar.printer.Printer.NodePrinter
import core.parsers.CommonParserWriter

trait CustomGrammarWithoutChildren extends BiGrammar with NodePrinter with CommonParserWriter {
  def getParser(keywords: scala.collection.Set[String]): Parser[Any]
}
