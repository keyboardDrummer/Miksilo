package core.bigrammar.grammars

import core.bigrammar.printer.Printer
import core.bigrammar.{BiGrammar, BiGrammarToParser, WithMap}
import BiGrammarToParser._

class BiFallback(value: Any, name: String) extends CustomGrammarWithoutChildren with BiGrammarWithoutChildren {
  override def getParserBuilder(keywords: collection.Set[String]) = Fallback(value, name)

  override def containsParser(recursive: BiGrammar => Boolean) = false

  override def write(from: WithMap[Any]) = Printer.fail("fallback cannot print")
}
