package core.bigrammar.grammars

import core.bigrammar.printer.TryState.State
import core.bigrammar.{BiGrammar, WithMapG}
import core.document.Empty

import scala.util.Try

object ParseWhiteSpace extends CustomGrammar with BiGrammarWithoutChildren {
  override def getGrammar = core.grammar.RegexG("""\s*""".r)

  override def write(from: WithMapG[Any], state: State) = Try(state, Empty)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = true
}
