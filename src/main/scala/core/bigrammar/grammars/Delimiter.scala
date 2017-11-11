package core.bigrammar.grammars

import core.bigrammar.BiGrammar

case class Delimiter(value: String) extends BiGrammarWithoutChildren {
  override def containsParser(recursive: BiGrammar => Boolean): Boolean = true
}
