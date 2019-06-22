package core.bigrammar.grammars

import core.bigrammar.BiGrammar.State
import core.bigrammar.BiGrammarToParser._
import core.bigrammar.printer.Printer
import core.bigrammar.{BiGrammar, WithMap}

class BiFallback(value: Any, name: String) extends CustomGrammarWithoutChildren with BiGrammarWithoutChildren {
  override def getParserBuilder(keywords: collection.Set[String]) = Fallback(value, name)

  override def containsParser(recursive: BiGrammar => Boolean) = false

  override def write(from: WithMap[Any], state: State) = Printer.fail("fallback cannot print")
}
