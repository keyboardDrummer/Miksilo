package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser._
import core.bigrammar.printer.Printer
import core.bigrammar.{BiGrammar, WithMap}

class BiFallback[Value](value: Value, name: String) extends CustomGrammarWithoutChildren[Value] with BiGrammarWithoutChildren[Value] {
  override def getParserBuilder(keywords: collection.Set[String]) = Fallback(value, name)

  override def containsParser(recursive: BiGrammar[_] => Boolean) = false

  override def write(from: Value) = Printer.fail("fallback cannot print")
}
