package core.bigrammar.grammars

import core.bigrammar.printer.TryState
import core.bigrammar.{BiGrammar, WithMapG}

object StringLiteral extends CustomGrammarWithoutChildren with BiGrammarWithoutChildren {
  override def getGrammar = core.grammar.StringLiteral
  override def write(from: WithMapG[Any]) = TryState.value("\"" + from.value + "\"")
  override def containsParser(recursive: BiGrammar => Boolean): Boolean = true
}
