package core.bigrammar.grammars

import core.bigrammar.printer.TryState
import core.bigrammar.{BiGrammarToParser, WithMapG}

object StringLiteral extends StringGrammar {
  override def getParser(keywords: Set[String]): BiGrammarToParser.Parser[Any] = BiGrammarToParser.stringLiteral ^^ (s => s.dropRight(1).drop(1))
  override def write(from: WithMapG[Any]) = TryState.value("\"" + from.value + "\"")
}
