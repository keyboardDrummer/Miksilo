package core.bigrammar.grammars

import core.bigrammar.printer.Printer
import core.bigrammar.{BiGrammar, BiGrammarToParser}

trait CustomGrammarWithoutChildren[Value] extends BiGrammar[Value] with Printer[Value] with BiGrammarWithoutChildren[Value] {
  def getParserBuilder(keywords: scala.collection.Set[String]): BiGrammarToParser.Self[Any]
}
