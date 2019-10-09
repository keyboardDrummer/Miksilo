package core.bigrammar.grammars

import core.bigrammar.BiGrammar

case class BiFailure(message: String = "") extends BiGrammarWithoutChildren {
  override def containsParser(recursive: BiGrammar => Boolean): Boolean = true
}
