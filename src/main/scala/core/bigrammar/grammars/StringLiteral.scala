package core.bigrammar.grammars

import core.bigrammar.printer.TryState.State
import core.bigrammar.{BiGrammar, WithMapG}

import scala.util.Try

object StringLiteral extends CustomGrammarWithoutChildren with BiGrammarWithoutChildren {
  override def getGrammar = core.grammar.StringLiteral
  override def write(from: WithMapG[Any], state: State) = Try(state, "\"" + from.value + "\"")
  override def containsParser(recursive: BiGrammar => Boolean): Boolean = true
}
